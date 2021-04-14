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
            
        } else {
            
            req(input$mex_state)
            
            aux_data <- filter(states_municipios, CVE_ENT %in% input$mex_state)
            
            c_zones <- aux_data$CVE_CZ
            
            names(c_zones) <- aux_data$NOM_CZ
            
            selectInput(
                inputId = "mex_cz",
                label = "Mexican Commuting Zone:",
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
            
            counties <- aux_data$COUNTYFP
            
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
            
            if(!is.null(input$mex_cz)){
                
                aux_data <- states_municipios %>% 
                    filter(CVE_ENT %in% input$mex_state, CVE_CZ %in% input$mex_cz)
                
            } else {
                
                aux_data <- filter(states_municipios, CVE_ENT %in% input$mex_state)
            }
            
            municipios <- aux_data$CVE_MUN
            
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
    
    main_map_data <- eventReactive(input$go, {
        
        if(input$by == "Destination"){
            
            req(input$us_state)
            
            shapefile <- mex_municipios
            
            country_frequencies <- mex_frequencies
            
            if(!is.null(input$us_cz)){
                
                ids <- filter(raw_data, CZ %in% input$us_cz)$GEOID_US
                
            } else {
                
                ids <- filter(raw_data, STATEFP %in% input$us_state)$GEOID_US
                
            }
            
        } else {
            
            req(input$mex_state)
            
            shapefile <- us_counties
            
            country_frequencies <- us_frequencies
            
            if(!is.null(input$mex_cz)){
                
                ids <- filter(raw_data, CVE_CZ %in% input$mex_cz)$GEOID_MX
                
            } else {
                
                ids <- filter(raw_data, CVE_ENT %in% input$mex_state)$GEOID_MX
                
            }
            
        }
        
        migrations <- filter_data(raw_data, ids, by = input$by) %>% 
            # TODO: incorporate level input
            filter(LEVEL == "County/Municipio") %>% 
            # wt.x: share of total matriculas accounted for by that source/destination
            # wt.y: destination/source share of overall MCAS
            left_join(y = country_frequencies,
                      by = c("LEVEL", "GEOID"))
        
        shapefile <- shapefile[shapefile$GEOID %in% migrations$GEOID, ]
        
        shapefile@data <- shapefile@data %>%
            left_join(y = migrations, by = "GEOID")
        
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
        
        data <- main_map_data()@data
        
        n_max <- round(max(data$wt.x), digits = 0) + .5
        
        bins <- seq(from = 0, to = n_max, by = n_max / 5)
        
        pal <- colorBin("Blues", domain = data$wt.x, bins = bins)
        
        if(input$by == "Destination"){
            states_map <- mex_states
        } else {
            states_map <- us_states
        }
        
        labs <- paste0(
            "<b>", data$NAME, " (", data$STATEABBR, ")</b><br>",
            "w.r.t ", input$by, ": ",
            format(data$migrations, nsmall = 0, big.mark = ","), " migrations (", round(data$wt.x, 1), "%)",
            "<br>",
            "w.r.t all MCAS: ",
            format(data$n_total, nsmall = 0, big.mark = ","), " migrations (", round(data$wt.y, 1), "%)"
        ) %>% as.list()
        
        leafletProxy(mapId = "map", data = main_map_data()) %>%
            addPolygons(
                fillColor = ~ pal(wt.x),
                label = lapply(labs, HTML),
                weight = 1,
                opacity = 1,
                color = "black",
                dashArray = "1",
                fillOpacity = 0.85,
                layerId = ~ GEOID
            ) %>%
            addLegend(
                pal = pal,
                values = ~wt.x,
                opacity = 0.85,
                position = "bottomright",
                title = "Migrations Percentages",
                labFormat = labelFormat(suffix = "%")
            ) %>%
            addPolylines(
                data = states_map,
                color = "black",
                opacity = 1,
                weight = 2
            )
    })
    
    
    # -- display total number of migrations for selected filter
    output$n_total <- renderText({
        
        req(input$by)
        
        n_total <- sum(main_map_data()@data$migrations)
        
        share <- n_total / total_matriculas * 100
        
        paste0("No. Matriculas for ", input$by, ": ", scales::comma(n_total), " (", round(share, 1), "%)")
    })

    # -- secondary data
    sec_map_data <- eventReactive(input$map_shape_click, {

        county_id <- input$map_shape_click$id

        if(input$by == "Destination"){
            
            shapefile <- us_counties
            
            country_frequencies <- us_frequencies
            
            by <- "Source"
            
        } else {
            
            shapefile <- mex_municipios
            
            country_frequencies <- mex_frequencies
            
            by <- "Destination"
            
        }
        
        migrations <- filter_data(raw_data, ids = county_id, by = by) %>% 
            # TODO: incorporate level input
            filter(LEVEL == "County/Municipio") %>% 
            # wt.x: share of total matriculas accounted for by that source/destination
            # wt.y: destination/source share of overall MCAS
            left_join(y = country_frequencies,
                      by = c("LEVEL", "GEOID"))
        
        shapefile <- shapefile[shapefile$GEOID %in% migrations$GEOID, ]
        
        shapefile@data <- shapefile@data %>%
            left_join(y = migrations, by = "GEOID")
        
        return(shapefile)
        
    })

    # # -- display title for secondary map
    # # output$title2 <- renderText({
    # #
    # #     req(input$map_shape_click$id)
    # #
    # #     county_id <- input$map_shape_click$id
    # #
    # #     if(input$by == "Destination"){
    # #
    # #         title <- raw_data %>%
    # #             mutate(ID = paste0(cve_ent, cve_mun)) %>%
    # #             filter(ID == county_id) %>%
    # #             mutate(text = paste0(nom_mun, ", ", nom_ent))
    # #
    # #         title <- title$text[1]
    # #
    # #         paste0("Destinations for Migrants born in ", title)
    # #
    # #     } else {
    # #
    # #         title <- raw_data %>%
    # #             mutate(ID = paste0(statefip, countyfip)) %>%
    # #             filter(ID == county_id) %>%
    # #             mutate(text = paste0(county, ", ", state))
    # #
    # #         title <- title$text[1]
    # #
    # #         paste0("Source Regions for Migrants in ", title)
    # #     }
    # #
    # # })

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
        
        data <- sec_map_data()@data
        
        n_max <- round(max(data$wt.x), digits = 0) + .5
        
        bins <- seq(from = 0, to = n_max, by = n_max / 5)
        
        pal <- colorBin("Blues", domain = data$wt.x, bins = bins)
        
        if(input$by == "Destination"){
            states_map <- us_states
        } else {
            states_map <- mex_states
        }
        
        labs <- paste0(
            "<b>", data$NAME, " (", data$STATEABBR, ")</b><br>",
            "w.r.t ", input$by, ": ",
            format(data$migrations, nsmall = 0, big.mark = ","), " migrations (", round(data$wt.x, 1), "%)",
            "<br>",
            "w.r.t all MCAS: ",
            format(data$n_total, nsmall = 0, big.mark = ","), " migrations (", round(data$wt.y, 1), "%)"
        ) %>% as.list()
        
        leafletProxy(mapId = "map2", data = sec_map_data()) %>%
            addPolygons(
                fillColor = ~ pal(wt.x),
                label = lapply(labs, HTML),
                weight = 1,
                opacity = 1,
                color = "black",
                dashArray = "1",
                fillOpacity = 0.85,
                layerId = ~ GEOID
            ) %>%
            addLegend(
                pal = pal,
                values = ~wt.x,
                opacity = 0.85,
                position = "bottomright",
                title = "Migrations Percentages",
                labFormat = labelFormat(suffix = "%")
            ) %>%
            addPolylines(
                data = states_map,
                color = "black",
                opacity = 1,
                weight = 2
            )

    })

    # -- display total number of migrations for secondary map
    output$n_total2 <- renderText({
        
        req(input$map_shape_click$id)

        county_id <- as.character(input$map_shape_click$id)

        if(input$by == "Destination"){

            name <- filter(raw_data, GEOID_MX == county_id)$NOM_MUN[1]            

        } else {

            name <- filter(raw_data, GEOID_US == county_id)$COUNTY[1]
        }

        n_total <- sum(sec_map_data()@data$migrations)

        share <- n_total / total_matriculas * 100

        paste0("No. Matriculas for ", name, ": ", scales::comma(n_total), " (", round(share, 1), "%)")
    
    })
    
}