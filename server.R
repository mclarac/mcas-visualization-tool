library("shiny")
library("tidyr")
library("dplyr")
library("stringr")
library("scales")
library("sp")
library("leaflet")
library("leaflet.extras")
library("mapview")
library("htmlwidgets")

# -- import helper functions
source(file = 'utils.R')

# -- load cleaned and transformed data
load(file = 'cleaned_data.RData')

# -- helpers
levels <- c("State", "Commuting zone", "County/Municipio")

states_counties <- raw_data %>% 
    select(GEOID_US, STATEFP, STATE, CZ, CZ_NAME, COUNTYFP, COUNTY) %>%
    distinct() %>% 
    arrange(STATE, CZ_NAME, COUNTY)

us_frequencies <- get_frequencies(data = raw_data, levels = levels)

states_municipios <- raw_data %>% 
    select(GEOID_MX, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) %>%
    distinct() %>% 
    arrange(NOM_ENT, NOM_MUN)

mex_frequencies <- get_frequencies(data = raw_data, levels = levels, country = "mex")

total_matriculas <- sum(raw_data$MATRICULA)

# -- input choices
us_states_choices <- states_counties$STATEFP %>% unique()

names(us_states_choices) <- states_counties$STATE %>% unique()

mex_states_choices <- states_municipios$CVE_ENT %>% unique()

names(mex_states_choices) <- states_municipios$NOM_ENT %>% unique()

server <- function(input, output, session) {
    
    # -- dynamic dropdown lists for both US and Mexico
    output$opts <- renderUI({
        
        if(input$by == "Destination"){
            
            selectInput(
                inputId = "us_state", 
                label = "U.S. State:",
                choices = us_states_choices,
                multiple = TRUE
            )
            
        } else {
            
            selectInput(
                inputId = "mex_state", 
                label = "Mexico State:",
                choices = mex_states_choices,
                multiple = TRUE
            )
        }
    })
    
    output$opts2 <- renderUI({
        
        if(input$by == "Destination"){
            
            req(input$us_state)
            
            aux_data <- filter(states_counties, STATEFP %in% input$us_state)
            
            c_zones <- aux_data$CZ
            
            names(c_zones) <- aux_data$CZ_NAME
            
            selectInput(
                inputId = "us_cz",
                label = "U.S. Commuting Zone:",
                choices = c_zones,
                multiple = TRUE,
                selected = "All"
            )
            
        } 
    })
    
    output$opts3 <- renderUI({
        
        if(input$by == "Destination"){
            
            req(input$us_state)
            
            if(!is.null(input$us_cz)){
                
                aux_data <- states_counties %>% 
                    filter(STATEFP %in% input$us_state, CZ %in% input$us_cz)
                
            } else {
                
                aux_data <- filter(states_counties, STATEFP %in% input$us_state)
            }
            
            counties <- aux_data$GEOID_US
            
            names(counties) <- aux_data$COUNTY
            
            selectInput(
                inputId = "us_county",
                label = "U.S. County:",
                choices = counties,
                multiple = TRUE,
                selected = "All"
            )
            
        } else {
            
            req(input$mex_state)
            
            aux_data <- filter(states_municipios, CVE_ENT %in% input$mex_state)
            
            municipios <- aux_data$GEOID_MX
            
            names(municipios) <- aux_data$NOM_MUN
            
            selectInput(
                inputId = "municipio",
                label = "Mexican Municipio:",
                choices = municipios,
                multiple = TRUE,
                selected = "All"
            )
        }
    })
    
    output$opts4 <- renderUI({
        
        if(input$by == "Destination"){
            
            radioButtons(
                inputId = "level", 
                label = "Level of geographic aggregation:", 
                choices = levels[!levels %in% "Commuting zone"], 
                inline = TRUE,
                selected = levels[1]
            )
            
        } else {
            
            radioButtons(
                inputId = "level", 
                label = "Level of geographic aggregation:", 
                choices = levels, 
                inline = TRUE,
                selected = levels[1]
            )
        }
    })
    
    output$opts5 <- renderUI({
        
        if(input$by == "Destination"){
            
            radioButtons(
                inputId = "level2", 
                label = "Level of geographic aggregation:", 
                choices = levels, 
                inline = TRUE,
                selected = levels[1]
            )
            
        } else {
            
            radioButtons(
                inputId = "level2", 
                label = "Level of geographic aggregation:", 
                choices = levels[!levels %in% "Commuting zone"], 
                inline = TRUE,
                selected = levels[1]
            )
        }
    })
    
    main_map_data <- reactive({
        
        req(input$level)
        
        if(input$by == "Destination"){
            
            req(input$us_state)
            
            if(input$level == "County/Municipio") shapefile <- mex_municipios else shapefile <- mex_states
            
            country_frequencies <- mex_frequencies
            
            
            if(!is.null(input$us_county)){
                
                ids <- input$us_county
                
            } else if(!is.null(input$us_cz)){
                
                ids <- filter(raw_data, CZ %in% input$us_cz)$GEOID_US
                
            } else {
                
                ids <- filter(raw_data, STATEFP %in% input$us_state)$GEOID_US
            }
            
        } else {
            
            req(input$mex_state)
            
            if(input$level == "County/Municipio"){
                
                shapefile <- us_counties
                
            } else if(input$level == "State"){
                
                shapefile <- us_states
                
            } else {
                
                shapefile <- us_cz
            }
            
            country_frequencies <- us_frequencies
            
            if(!is.null(input$municipio)){
                
                ids <- input$municipio
                
            } else {
                
                ids <- filter(raw_data, CVE_ENT %in% input$mex_state)$GEOID_MX
                
            }
            
        }
        
        migrations <- filter_data(raw_data, ids = ids, levels = levels, by = input$by) %>% 
            filter(LEVEL == input$level) %>% 
            # wt.x: share of total matriculas accounted for by that source/destination
            # wt.y: destination/source share of overall MCAS
            left_join(y = country_frequencies,
                      by = c("LEVEL", "GEOID"))
        
        shapefile <- shapefile[shapefile$GEOID %in% migrations$GEOID, ]
        
        shapefile@data <- shapefile@data %>%
            inner_join(y = migrations, by = "GEOID")
        
        return(shapefile)
    })
    
    # -- main map
    map_reactive <- reactive({
        
        req(main_map_data())
        
        shapefile <- main_map_data()
        
        leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            setView(
                zoom = 4,
                # change latitude and longitude based on inputs
                lat = mean(coordinates(shapefile)[,2]),
                lng = mean(coordinates(shapefile)[,1])
            )
    })
    
    output$map <- renderLeaflet({
        map_reactive()
    })
    
    # -- replace layer according to user inputs
    observe({
        
        req(main_map_data())
        
        leafletProxy(mapId = "map") %>% 
            create_map(
                map_data = main_map_data(), 
                by = input$by,
                palette = "Blues",
                mex_states = mex_states, 
                us_states = us_states
            )
    })
    
    
    # -- display total number of migrations for selected filter
    output$n_total <- renderText({
        
        req(input$by)
        
        n_total <- sum(main_map_data()@data$migrations)
        
        share <- n_total / total_matriculas * 100
        # TODO: when level is commuting zone, the number of matriculas decreases
        paste0("No. Matriculas for ", input$by, ": ", comma(n_total), " (", round(share, 1), "%)")
        
    })
    
    # download handler for map 1
    output$downloadMap1 <- downloadHandler(
        
        filename = function() {
            paste("map1-", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
            saveWidget(
                widget = map_reactive() %>% 
                    create_map(
                        map_data = , 
                        by = input$by,
                        palette = "Blues",
                        mex_states = mex_states, 
                        us_states = us_states
                    ), 
                file = file
            )
        }
    )
    
    # extract data from shapefile and select only some columns
    data1 <- reactive({
        
        data <- main_map_data()@data %>% 
            select(STATENAME = NAME,
                   !!paste0("MIGRATIONS_", input$by) := migrations,
                   !!paste0("PERCENT_WRT_", input$by) := wt.x,
                   TOTAL_MIGRATIONS = n_total,
                   PERCENT_WRT_ALL = wt.y
               )
        
        names(data) <- names(data) %>% tolower()
        
        return(data)
    })
    
    # download handler for map data 1
    output$downloadData1 <- downloadHandler(
        
        filename = function() {
            paste("data1-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(x = data1(), file = file, row.names = FALSE)
        }
    )
    
    output$aux <- renderText({
        input$map_shape_click$id
    })
    
    # -- secondary data: updated when user clicks on a location on the map on the LHS
    sec_map_data <- reactive({
        
        req(input$map_shape_click)
        
        req(input$level2)
        
        if(input$by == "Destination"){
            
            req(input$us_state)
            
            if(input$level2 == "County/Municipio"){
                
                shapefile <- us_counties
                
            } else if(input$level2 == "State"){
                
                shapefile <- us_states
                
            } else {
                
                shapefile <- us_cz
            }
            
            country_frequencies <- us_frequencies
            
            by <- "Source"
            
        } else {
            
            req(input$mex_state)
            
            if(input$level2 == "County/Municipio") shapefile <- mex_municipios else shapefile <- mex_states
            
            country_frequencies <- mex_frequencies
            
            by <- "Destination"
            
        }
        
        id <- input$map_shape_click$id
        
        # input$level <- "County/Municipio"
        
        if(input$level != "County/Municipio"){
            
            id <- get_ids(raw_data, id = id, by = input$by, level = input$level)
        }
        
        migrations <- filter_data(raw_data, ids = id, levels = levels, by = by) %>%
            filter(LEVEL == input$level2) %>%
            # wt.x: share of total matriculas accounted for by that source/destination
            # wt.y: destination/source share of overall MCAS
            left_join(y = country_frequencies,
                      by = c("LEVEL", "GEOID"))
        
        # when user changes level from LHS map, the data is empty until a new location is selected
        validate(
            need(nrow(migrations) > 0, "Please select a location to update the map")
        )
        
        shapefile <- shapefile[shapefile$GEOID %in% migrations$GEOID, ]
        
        shapefile@data <- shapefile@data %>%
            left_join(y = migrations, by = "GEOID")
        
        return(shapefile)
        
    })
    
    # -- secondary map based on user clicks on main map
    map_reactive2 <- reactive({
        
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
    
    output$map2 <- renderLeaflet({
        map_reactive2()
    })
    
    # -- replace layer according to user inputs
    observe({
        
        req(sec_map_data())
        
        leafletProxy(mapId = "map2") %>% 
            create_map(
                map_data = sec_map_data(), 
                by = input$by,
                primary = FALSE,
                palette = "Oranges",
                mex_states = mex_states, 
                us_states = us_states
            )
    })
    
    # -- display total number of migrations for secondary map
    output$n_total2 <- renderText({
        
        req(input$map_shape_click$id)
        
        id <- as.character(input$map_shape_click$id)
        
        if(input$by == "Destination"){
            
            if(input$level == "State"){
                
                name <- filter(raw_data, CVE_ENT == id)$NOM_ENT[1]
                
            } else {
                
                name <- filter(raw_data, GEOID_MX == id)$NOM_MUN[1]
            }
            
        } else {
            
            if(input$level == "State"){
                
                name <- filter(raw_data, STATEFP == id)$STATE[1]
                
            } else if(input$level == "Commuting zone"){
                
                name <- filter(raw_data, CZ == id)$CZ_NAME[1]
                
            } else {
                
                name <- filter(raw_data, GEOID_US == id)$COUNTY[1]
            }
            
        }
        
        n_total <- sum(sec_map_data()@data$migrations)
        
        # sum(shapefile@data$migrations)
        
        share <- n_total / total_matriculas * 100
        
        paste0("No. Matriculas for ", name, ": ", comma(n_total), " (", round(share, 1), "%)")
        
    })
    
    # download handler for map 1
    output$downloadMap2 <- downloadHandler(
        
        filename = function() {
            paste("map2-", Sys.Date(), ".html", sep="")
        },
        content = function(file) {
            saveWidget(
                widget = map_reactive2() %>% 
                    create_map(
                        map_data = sec_map_data(), 
                        by = input$by,
                        primary = FALSE,
                        palette = "Oranges",
                        mex_states = mex_states, 
                        us_states = us_states
                    ), 
                file = file
            )
        }
    )
    
    data2 <- reactive({
        
        data <- sec_map_data()@data %>% 
            select(STATENAME = NAME,
                   !!paste0("MIGRATIONS_", input$by) := migrations,
                   !!paste0("PERCENT_WRT_", input$by) := wt.x,
                   TOTAL_MIGRATIONS = n_total,
                   PERCENT_WRT_ALL = wt.y
            )
        
        names(data) <- names(data) %>% tolower()
        
        return(data)
    })
    
    # download handler for map 2
    output$downloadData2 <- downloadHandler(
        
        filename = function() {
            paste("data2-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(x = data2(), file = file, row.names = FALSE)
        }
    )
    
    output$aux2 <- renderText({
        input$map2_shape_click$id
    })
    
}