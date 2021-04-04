library("shiny")
library("shinythemes")
library("DT")

library("tidyr")
library("dplyr")
library("stringr")
library("scales")
library("readr")
library("rgdal")
library("sf")
library("leaflet")
library("leaflet.extras")

# -- data loading
file <- './mcas_2008.csv'
raw_data <- read_csv(
    file = file, 
    col_types = cols(
        nom_mun = col_character(),
        nom_ent = col_character(),
        county = col_character(),
        state = col_character(),
        .default = col_double()
    )
)

raw_data <- raw_data %>% 
    mutate_at(.vars = c("statefip", "cve_ent"),
              .funs = function(x) str_pad(x, width = 2, pad = "0")) %>% 
    mutate_at(.vars = c("countyfip", "cve_mun"),
              .funs = function(x) str_pad(x, width = 3, pad = "0")) %>% 
    # there are several counties/municipios with the same names
    # so we need to make sure which one is being selected in the dropdown lists
    mutate(county = paste0(statefip, ": ", county),
           nom_mun = paste0(cve_ent, ": ", nom_mun)) %>% 
    rename(statefp = statefip, countyfp = countyfip)

names(raw_data) <- names(raw_data) %>% toupper()

load(file = "shapefiles.RData")

# shapefiles transformations
mex_states@data <- mex_states@data %>%
    # remove MX for every GMI_ADMIN and extract only numbers from CVE_ENT
    # so variable values are consistent with raw_data
    mutate(STATEABBR = str_sub(GMI_ADMIN, start = 5),
           CVE_ENT = str_extract(FIPS_ADMIN, pattern = "[[:digit:]]+"))

mex_municipios@data <- mex_municipios@data %>% 
    rename(NAME = NOM_MUN)

us_states@data <- us_states@data %>% 
    rename(STATEABBR = STUSPS)

# -- helpers

us_states_choices <- raw_data$STATE %>% unique() %>% sort()

mex_states_choices <- raw_data$NOM_ENT %>% unique() %>% sort()

states_counties <- raw_data %>% 
    select(STATE, COUNTY) %>%
    distinct() %>% 
    arrange(COUNTY)

states_municipios <- raw_data %>% 
    select(NOM_ENT, NOM_MUN) %>%
    distinct() %>% 
    arrange(NOM_MUN)

# --- GUI
ui <- fluidPage(
    
    titlePanel(title = "Migrant Destinations & Sources Distributions"),
    
    br(),
    
    # themeSelector(),
    
    theme = shinytheme("sandstone"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            width = 2,
            
            radioButtons(
                inputId = "by", 
                label = "Analyze by:", 
                choices = c("Source", "Destination"), 
                inline = TRUE
            ),
            
            hr(),
            
            uiOutput(outputId = "opts"),
            
            uiOutput(outputId = "opts2")
        ),
        
        mainPanel(
            width = 10,
            
            fluidRow(
                column(
                    
                    width = 6, 
                    
                    # h4(textOutput(outputId = "title")),
                    
                    leafletOutput(outputId = "map", height = "600px"),
                    
                    textOutput(outputId = "n_total", inline = TRUE)
                ),
                
                column(
                    
                    width = 6,
                    
                    # h4(textOutput(outputId = "title2")),
                    
                    leafletOutput(outputId = "map2", height = "600px"),
                    
                    textOutput(outputId = "n_total2", inline = TRUE)
                )
            )
        )
    )
)


server <- function(input, output, session) {
    
    
    output$opts <- renderUI({
        
        
        if(input$by == "Destination"){
            
            selectInput(
                inputId = "us_state", 
                label = "U.S. state:",
                choices = us_states_choices,
                multiple = TRUE
            )
            
        } else {
            
            selectInput(
                inputId = "mex_state", 
                label = "Mexico state:",
                choices = mex_states_choices,
                multiple = TRUE
            )
        }
    })
    
    output$opts2 <- renderUI({
        
        if(input$by == "Destination"){
            
            req(input$us_state)
            
            selectInput(
                inputId = "us_county",
                label = "U.S. county:",
                choices = c("All", filter(states_counties, STATE == input$us_state)$COUNTY),
                multiple = FALSE
            )
            
        } else {
            
            req(input$mex_state)
            
            selectInput(
                inputId = "municipio",
                label = "Mexican municipio:",
                choices = c("All", filter(states_municipios, NOM_ENT == input$mex_state)$NOM_MUN),
                multiple = FALSE
            )
            
        }
        
    })
    
    migrations_df <- reactive({
        
        if(input$by == "Destination") {
            
            req(input$us_state)
            
            req(input$us_county)
            
            df <- raw_data %>%
                filter(STATE %in% input$us_state)
            
            if(input$us_county != "All"){
                
                df <- df %>% 
                    filter(COUNTY == input$us_county) %>% 
                    count(CVE_ENT, CVE_MUN, name = "migrations")
                
            } else {
                
                df <- count(x = df, CVE_ENT, CVE_MUN, name = "migrations")
            }
            
        } else {
            
            req(input$mex_state)
            
            req(input$municipio)
            
            df <- raw_data %>%
                filter(NOM_ENT %in% input$mex_state)
            
            if(input$municipio != "All"){
                
                df <- df %>% 
                    filter(NOM_MUN == input$municipio) %>% 
                    count(STATEFP, COUNTYFP, name = "migrations")
                
            } else {
                
                df <- count(x = df, STATEFP, COUNTYFP, name = "migrations")
                
            }
        }
        
        df %>% mutate(wt = migrations / sum(migrations) * 100)
    })
    
    main_map_data <- reactive({
        
        req(migrations_df())
        
        if(input$by == "Destination"){
            
            shapefile <- mex_municipios
            
            id_one <- paste0(shapefile$CVE_ENT, shapefile$CVE_MUN) 
            
            id_two <-  paste0(migrations_df()$CVE_ENT, migrations_df()$CVE_MUN)
            
            shapefile <- shapefile[id_one %in% id_two, ]
            
            shapefile@data <- shapefile@data %>%
                left_join(y = migrations_df(),
                          by = c("CVE_ENT", "CVE_MUN")) %>%
                left_join(y = mex_states@data[, c("CVE_ENT", "STATEABBR")], 
                          by = "CVE_ENT") %>% 
                rename(STATECD = CVE_ENT,
                       COUNTYCD = CVE_MUN)
            
        } else {
            
            shapefile <- us_counties
            
            id_one <- paste0(shapefile$STATEFP, shapefile$COUNTYFP)
            
            id_two <- paste0(migrations_df()$STATEFP, migrations_df()$COUNTYFP)
            
            shapefile <- shapefile[id_one %in% id_two, ]
            
            shapefile@data <- shapefile@data %>%
                left_join(y = migrations_df(),
                          by = c("STATEFP", "COUNTYFP")) %>%
                left_join(y = us_states@data[, c("STATEFP", "STATEABBR")],
                          by = "STATEFP") %>% 
                rename(STATECD = STATEFP,
                       COUNTYCD = COUNTYFP)
        }
        
        return(shapefile)
    })
    
    # -- display title for main map
    # output$title <- renderText({
    #     
    #     "Some text"
    #     
    # })
    
    # -- main map
    output$map <- renderLeaflet({
        
        req(main_map_data())
        
        leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(
                zoom = 4,
                # change lat and lon based on inputs
                lat = mean(coordinates(main_map_data())[,2]),
                lng = mean(coordinates(main_map_data())[,1])
            )
    })
    
    # -- replace layer according to user inputs
    observe({
        
        req(main_map_data())
        
        n_max <- round(max(main_map_data()@data$wt), digits = 0) + .5
        
        bins <- seq(from = 0, to = n_max, by = n_max / 5)
        
        pal <- colorBin("Blues", domain = main_map_data()@data$wt, bins = bins)
        
        if(input$by == "Destination"){
            states_map <- mex_states
        } else {
            states_map <- us_states
        }
        
        leafletProxy(mapId = "map", data = main_map_data()) %>% 
            addPolygons(
                fillColor = ~ pal(wt),
                popup = ~ paste0(
                    "<b>", NAME, " (", STATEABBR, ") :</b> ",
                    format(migrations, nsmall = 1, big.mark = ","), " migration(s)"
                ),
                weight = 1,
                opacity = 1,
                color = "black",
                dashArray = "1",
                fillOpacity = 0.85,
                layerId = ~ paste0(STATECD, COUNTYCD)
            ) %>%
            addLegend(
                pal = pal,
                values = ~wt,
                opacity = 0.85,
                position = "bottomright",
                title = "Migrations Percentages",
                labFormat = labelFormat(suffix = "%")
            ) %>%
            addPolylines(
                data = states_map,
                color = "black",
                opacity = 1,
                weight = 3
            )
        
    })
    
    # -- display total number of migrations for selected filter
    output$n_total <- renderText({
        paste0("Matriculas: ", scales::comma(sum(main_map_data()@data$migrations)))
    })
    
    # -- secondary data
    sec_map_data <- eventReactive(input$map_shape_click, {
        
        county_id <- input$map_shape_click$id
        
        if(input$by == "Destination"){
            
            migrations <- raw_data %>% 
                mutate(ID = paste0(CVE_ENT, CVE_MUN)) %>% 
                filter(ID == county_id) %>% 
                count(STATEFP, COUNTYFP, name = "migrations") %>% 
                mutate(wt = migrations / sum(migrations) * 100)
            
            shapefile <- us_counties
            
            id_one <- paste0(shapefile$STATEFP, shapefile$COUNTYFP)
            
            id_two <- paste0(migrations$STATEFP, migrations$COUNTYFP)
            
            shapefile <- shapefile[id_one %in% id_two, ]
            
            shapefile@data <- shapefile@data %>%
                left_join(y = migrations,
                          by = c("STATEFP", "COUNTYFP")) %>%
                left_join(y = us_states@data[, c("STATEFP", "STATEABBR")],
                          by = "STATEFP")
            
        } else {
            
            migrations <- raw_data %>% 
                mutate(ID = paste0(STATEFP, COUNTYFP)) %>% 
                filter(ID == county_id) %>% 
                count(CVE_ENT, CVE_MUN, name = "migrations") %>% 
                mutate(wt = migrations / sum(migrations) * 100)
            
            shapefile <- mex_municipios
            
            id_one <- paste0(shapefile$CVE_ENT, shapefile$CVE_MUN)
            
            id_two <- paste0(migrations$CVE_ENT, migrations$CVE_MUN)
            
            shapefile <- shapefile[id_one %in% id_two, ]
            
            shapefile@data <- shapefile@data %>%
                left_join(y = migrations,
                          by = c("CVE_ENT", "CVE_MUN")) %>%
                left_join(y = mex_states@data[, c("CVE_ENT", "STATEABBR")], 
                          by = "CVE_ENT")
        }
        
        return(shapefile)
    })
    
    # -- display title for secondary map
    # output$title2 <- renderText({
    #     
    #     req(input$map_shape_click$id)
    #     
    #     county_id <- input$map_shape_click$id
    #     
    #     if(input$by == "Destination"){
    #         
    #         title <- raw_data %>% 
    #             mutate(ID = paste0(cve_ent, cve_mun)) %>% 
    #             filter(ID == county_id) %>% 
    #             mutate(text = paste0(nom_mun, ", ", nom_ent))
    #         
    #         title <- title$text[1] 
    #         
    #         paste0("Destinations for Migrants born in ", title)
    #         
    #     } else {
    #         
    #         title <- raw_data %>% 
    #             mutate(ID = paste0(statefip, countyfip)) %>% 
    #             filter(ID == county_id) %>% 
    #             mutate(text = paste0(county, ", ", state))
    #         
    #         title <- title$text[1] 
    #         
    #         paste0("Source Regions for Migrants in ", title)
    #     }
    #     
    # })
    
    # -- secondary map based on user clicks on main map
    output$map2 <- renderLeaflet({
        
        req(sec_map_data())
        
        leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(
                zoom = 5,
                # change lat and lon based on inputs
                lat = mean(coordinates(sec_map_data())[,2]),
                lng = mean(coordinates(sec_map_data())[,1])
            )
    })
    
    # -- replace layer according to user inputs
    observe({
        
        req(sec_map_data())
        
        n_max <- round(max(sec_map_data()@data$wt), digits = 0) + .5
        
        bins <- seq(from = 0, to = n_max, by = n_max / 5)
        
        pal <- colorBin("Purples", domain = sec_map_data()@data$wt, bins = bins)
        
        if(input$by == "Destination"){
            states_map <- us_states
        } else {
            states_map <- mex_states
        }
        
        leafletProxy(mapId = "map2", data = sec_map_data()) %>% 
            addPolygons(
                fillColor = ~ pal(wt),
                popup = ~ paste0(
                    "<b>", NAME, " (", STATEABBR, ") :</b> ",
                    format(migrations, nsmall = 1, big.mark = ","), " migration(s)"
                ),
                weight = 1,
                opacity = 1,
                color = "black",
                dashArray = "1",
                fillOpacity = 0.85
            ) %>%
            addLegend(
                pal = pal,
                values = ~wt,
                opacity = 0.85,
                position = "bottomright",
                title = "Migrations Percentages",
                labFormat = labelFormat(suffix = "%")
            ) %>%
            addPolylines(
                data = states_map,
                color = "black",
                opacity = 1,
                weight = 3
            )
        
    })
    
    # -- display total number of migrations for secondary map
    output$n_total2 <- renderText({
        paste0("Matriculas: ", scales::comma(sum(sec_map_data()@data$migrations)))
    })

}

# run the application
shinyApp(ui = ui, server = server)