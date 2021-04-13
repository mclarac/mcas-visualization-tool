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
            
            if(!is.null(input$us_cz)){
                
                ids <- filter(raw_data, CZ %in% input$us_cz)$GEOID_US
                
            } else {
                
                ids <- filter(raw_data, COUNTYFP %in% input$us_state)$GEOID_US
                
            }
            
        } else {
            
            req(input$mex_state)
            
            if(!is.null(input$mex_cz)){
                
                ids <- filter(raw_data, CVE_CZ %in% input$mex_cz)$GEOID_MX
                
            } else {
                
                ids <- filter(raw_data, CVE_ENT %in% input$mex_state)$GEOID_MX
                
            }
        }
        
        migrations <- filter_data(raw_data, ids, by = input$by) %>% 
            # TODO: incorporate level input
            filter(LEVEL == "County/Municipio")
        
        shapefile <- us_counties
        
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