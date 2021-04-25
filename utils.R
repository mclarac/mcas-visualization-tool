# -- data manipulation functions
get_ids <- function(data, id, by = "Source", level = "State"){
    
    if(by == "Source"){
        
        if(level == "State"){
            
            ids <- filter(raw_data, STATEFP == id)
            
        } else {
            
            ids <- filter(raw_data, CZ == id)
        }
        
        ids <- ids$GEOID_US %>% unique()
        
    } else {
        
        ids <- filter(raw_data, CVE_ENT == id)$GEOID_MX %>% unique()
        
    } 
}

get_frequencies <- function(data, country = "us"){
    
    if(country == "us"){
        
        frequencies <- data %>% 
            select(STATEFP, CZ, GEOID_US, MATRICULA) %>% 
            gather(LEVEL, GEOID, -MATRICULA) %>% 
            mutate(LEVEL = plyr::mapvalues(
                x = LEVEL, 
                from = c("STATEFP", "CZ", "GEOID_US"), 
                to = levels)
            )
        
    } else {
        
        frequencies <- data %>% 
            select(CVE_ENT, GEOID_MX, MATRICULA) %>% 
            gather(LEVEL, GEOID, -MATRICULA) %>% 
            mutate(LEVEL = plyr::mapvalues(
                x = LEVEL, 
                from = c("CVE_ENT", "GEOID_MX"), 
                to = levels[!levels %in% "Commuting zone"])
            )
        
    }
    
    frequencies <- frequencies %>% 
        group_by(LEVEL, GEOID) %>% 
        summarise(n_total = sum(MATRICULA), .groups = "drop") %>% 
        group_by(LEVEL) %>% 
        mutate(wt = n_total / sum(n_total) * 100)
    
    return(frequencies)
}

filter_data <- function(data, ids, by){
    
    if(by != "Source"){
        
        results <- data %>% 
            filter(GEOID_US %in% ids) %>% 
            get_frequencies(country = "mex")
        
    } else {
        
        results <- data %>% 
            filter(GEOID_MX %in% ids) %>% 
            get_frequencies(country = "us")
    }
    
    results <- results %>% 
        # wt: share of total matriculas accounted for by that source/destination
        rename(migrations = n_total)
    
    return(results)
}

create_map <- function(map, map_data, by, primary = TRUE, palette = "Blues"){
    
    data <- map_data@data
    
    n_max <- round(max(data$wt.x), digits = 0) + .5
    # 
    # bins <- round(seq(from = 0, to = n_max, length.out = 10), digits = 1)
    
    quantiles <- quantile(data$wt.x, probs = c(.03, .16, .5, .84, .97), names = FALSE)
    
    bins <- unique(round(c(0, quantiles, n_max), 2))
    
    pal <- colorBin(palette, domain = data$wt.x, bins = bins)
    
    if(by == "Destination"){
        if(primary) states_map <- mex_states else states_map <- us_states
    } else {
        if(primary) states_map <- us_states else states_map <- mex_states
    }
    
    labs <- paste0(
        "<b>", data$NAME, " (", data$STATEABBR, ")</b><br>",
        "w.r.t ", by, ": ",
        format(data$migrations, nsmall = 0, big.mark = ","), " migrations (", round(data$wt.x, 1), "%)",
        "<br>",
        "w.r.t all MCAS: ",
        format(data$n_total, nsmall = 0, big.mark = ","), " migrations (", round(data$wt.y, 1), "%)"
    ) %>% as.list()
    
    map %>%
        addPolygons(
            data = map_data,
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
            data = map_data,
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
}

