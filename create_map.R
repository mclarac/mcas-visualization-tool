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

