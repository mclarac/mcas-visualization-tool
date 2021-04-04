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
    mutate(county = paste0(statefip, ": ", county),
           nom_mun = paste0(cve_ent, ": ", nom_mun))

load(file = "shapefiles.RData")

# -- helpers

us_states_choices <- raw_data$state %>% unique() %>% sort()

mex_states_choices <- raw_data$nom_ent %>% unique() %>% sort()

states_counties <- raw_data %>% 
    distinct(state, county) %>% 
    arrange(state, county)

states_municipios <- raw_data %>% 
    distinct(nom_ent, nom_mun) %>% 
    arrange(nom_ent, nom_mun)

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
                    
                    leafletOutput(outputId = "map", height = "600px"),
                    
                    textOutput(outputId = "n_total", inline = TRUE)
                ),
                
                column(
                    
                    width = 6,
                    
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
                choices = c("All", filter(states_counties, state == input$us_state)$county),
                multiple = FALSE
            )
            
        } else {
            
            req(input$mex_state)
            
            selectInput(
                inputId = "municipio",
                label = "Mexican municipio:",
                choices = c("All", filter(states_municipios, nom_ent == input$mex_state)$nom_mun),
                multiple = FALSE
            )
            
        }
        
    })
    
    distributions_df <- reactive({
        
        if(input$by == "Destination") {
            
            req(input$us_state)
            
            req(input$us_county)
            
            df <- raw_data %>%
                filter(state %in% input$us_state)
            
            if(input$us_county != "All"){
                
                df <- df %>% 
                    filter(county == input$us_county) %>% 
                    count(cve_ent, cve_mun, name = "migrations")
                
            } else {
                
                df <- count(x = df, cve_ent, cve_mun, name = "migrations")
            }
            
        } else {
            
            req(input$mex_state)
            
            req(input$municipio)
            
            df <- raw_data %>%
                filter(nom_ent %in% input$mex_state)
            
            if(input$municipio != "All"){
                
                df <- df %>% 
                    filter(nom_mun == input$municipio) %>% 
                    count(statefip, countyfip, name = "migrations")
                
            } else {
                
                df <- count(x = df, statefip, countyfip, name = "migrations")
                
            }
        }
        
        df %>% mutate(wt = migrations / sum(migrations) * 100)
    })
    
    main_map_data <- reactive({
        
        req(distributions_df())
        
        if(input$by == "Destination"){
            
            shapefile <- mex_municipios
            
            mask <- paste0(shapefile$CVE_ENT, shapefile$CVE_MUN) %in% paste0(distributions_df()$cve_ent, distributions_df()$cve_mun)
            
            shapefile <- shapefile[mask, ]
            
            shapefile@data <- shapefile@data %>%
                left_join(y = distributions_df(),
                          by = c("CVE_ENT" = "cve_ent", "CVE_MUN" = "cve_mun")) %>%
                left_join(y = mex_states@data[, c("CVE_ENT", "GMI_ADMIN")], 
                          by = "CVE_ENT") %>% 
                rename(NAME = NOM_MUN, 
                       STATEABBR = GMI_ADMIN, 
                       STATECD = CVE_ENT,
                       COUNTYCD = CVE_MUN)
            
        } else {
            
            shapefile <- us_counties
            
            mask <- paste0(shapefile$STATEFP, shapefile$COUNTYFP) %in% paste0(distributions_df()$statefip, distributions_df()$countyfip)
            
            shapefile <- shapefile[mask, ]
            
            shapefile@data <- shapefile@data %>%
                left_join(y = distributions_df(),
                          by = c("STATEFP" = "statefip", "COUNTYFP" = "countyfip")) %>%
                left_join(y = us_states@data[, c("STATEFP", "STUSPS")],
                          by = "STATEFP") %>% 
                rename(STATEABBR = STUSPS, 
                       STATECD = STATEFP,
                       COUNTYCD = COUNTYFP)
        }
        
        return(shapefile)
    })
    
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
                title = "Migrations",
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
        paste0("Total Number of Migrations: ", scales::comma(sum(main_map_data()@data$migrations)))
    })
    
    
    # -- secondary map
    # output$map2 <- renderLeaflet({
    #     
    #     req(input$map_shape_click)
    #     
    #     leaflet() %>%
    #         addProviderTiles(provider = "CartoDB.Positron") %>%
    #         setView(
    #             zoom = 5,
    #             # change lat and lon based on inputs
    #             lat = mean(coordinates(main_map_data())[,2]),
    #             lng = mean(coordinates(main_map_data())[,1])
    #         )
    # })
    
    # -- data table only for testing
    # output$table <- renderDT({
    #     datatable(
    #         data = distributions_df(),
    #         options = list(pageLength = 50),
    #         rownames = FALSE
    #     )
    # })
}

# run the application
shinyApp(ui = ui, server = server)