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

# # -- U.S. map: destinations
# n_max <- round(max(us_counties@data$wt), digits = 0) + .5
# 
# bins <- seq(from = 0, to = n_max, by = n_max / 5)
# 
# pal <- colorBin("Blues", domain = us_counties@data$wt, bins = bins)
# 
# leaflet(data = us_counties) %>% 
#     addProviderTiles(provider = "Stamen.Toner") %>%
#     setView(
#         zoom = 4, 
#         lat = mean(coordinates(us_counties)[,2]), 
#         lng = mean(coordinates(us_counties)[,1])
#     ) %>% 
#     addPolygons(
#         fillColor = ~ pal(wt),
#         popup = ~ paste0(
#             "<b>", NAME, " (", STUSPS, ") :</b> ", 
#             format(migrations, nsmall = 1, big.mark = ","), " migration(s)"
#         ),
#         weight = 1,
#         opacity = 1,
#         color = "black",
#         dashArray = "1",
#         fillOpacity = 0.85
#     ) %>%
#     addLegend(
#         pal = pal,
#         values = ~wt,
#         opacity = 0.85,
#         position = "bottomright",
#         title = "Destinations", 
#         labFormat = labelFormat(suffix = "%")
#     ) %>% 
#     addPolylines(
#         data = us_states, 
#         color = "black", 
#         opacity = 1, 
#         weight = 3
#     )
# 
# # -- Mexico map: sources
# n_max <- round(max(mex_municipios@data$wt), digits = 0) + .5
# 
# bins <- seq(from = 0, to = n_max, by = n_max / 5)
# 
# pal <- colorBin("Blues", domain = mex_municipios@data$wt, bins = bins)
# 
# leaflet(data = mex_municipios) %>% 
#     addProviderTiles(provider = "Stamen.Toner") %>%
#     setView(
#         zoom = 4, 
#         lat = mean(coordinates(mex_municipios)[,2]), 
#         lng = mean(coordinates(mex_municipios)[,1])
#     ) %>% 
#     addPolygons(
#         fillColor = ~ pal(wt),
#         popup = ~ paste0(
#             "<b>", NOM_MUN, " (", GMI_ADMIN, ") :</b> ", 
#             format(migrations, nsmall = 1, big.mark = ","), " migration(s)"
#         ),
#         weight = 1,
#         opacity = 1,
#         color = "black",
#         dashArray = "1",
#         fillOpacity = 0.85
#     ) %>%
#     addLegend(
#         pal = pal,
#         values = ~wt,
#         opacity = 0.85,
#         position = "bottomright",
#         title = "Destinations", 
#         labFormat = labelFormat(suffix = "%")
#     ) %>% 
#     addPolylines(
#         data = mex_states, 
#         color = "black", 
#         opacity = 1, 
#         weight = 3
#     )

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
    
    titlePanel(title = "Migrant Destination & Sources Distributions"),
    
    br(),
    
    # themeSelector(),
    
    theme = shinytheme("sandstone"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            width = 2,
            
            radioButtons(
                inputId = "by", 
                label = "Analyze by:", 
                choices = c("Destination", "Source"), 
                inline = TRUE
            ),
            
            hr(),
            
            uiOutput(outputId = "opts"),
            
            uiOutput(outputId = "opts2")
        ),
        
        mainPanel(
            width = 10,
            
            leafletOutput(outputId = "map1", height = "600px"),
            
            # leafletOutput(outputId = "map2", height = "600px")
            
            # DTOutput("table")
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
                left_join(mex_states@data[, c("CVE_ENT", "GMI_ADMIN")], by = "CVE_ENT")
            
        } else {
            
            shapefile <- us_counties
            
            mask <- paste0(shapefile$STATEFP, shapefile$COUNTYFP) %in% paste0(distributions_df()$statefip, distributions_df()$countyfip)
            
            shapefile <- shapefile[mask, ]
            
            shapefile@data <- shapefile@data %>%
                left_join(y = distributions_df(),
                          by = c("STATEFP" = "statefip", "COUNTYFP" = "countyfip")) %>%
                left_join(us_states@data[, c("STATEFP", "STUSPS")], by = "STATEFP")
        }
        
        return(shapefile)
    })
    
    # -- basic map
    output$map1 <- renderLeaflet({
        
        req(main_map_data())
        
        leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(
                zoom = 5,
                # change lat and lon based on inputs
                lat = mean(coordinates(main_map_data())[,2]),
                lng = mean(coordinates(main_map_data())[,1])
            )
    })
    
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