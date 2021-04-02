library("tidyr")
library("dplyr")
library("stringr")
library("scales")
library("readr")
library("rgdal")
library("sf")
library("leaflet")

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
              .funs = function(x) str_pad(x, width = 3, pad = "0"))

destinations <- raw_data %>% # all Mexico
    count(statefip, countyfip, name = "migrations") %>% 
    mutate(wt = migrations / sum(migrations) * 100)

sources <- raw_data %>% # all Mexico
    count(cve_ent, cve_mun, name = "migrations") %>% 
    mutate(wt = migrations / sum(migrations) * 100)

# -- U.S. map: destinations
us_states <- readOGR('./shapefiles/cb_2016_us_state_5m/cb_2016_us_state_5m.shp')

us_counties <- readOGR('./shapefiles/cb_2016_us_county_5m/cb_2016_us_county_5m.shp')

mask <- paste0(us_counties$STATEFP, us_counties$COUNTYFP) %in% paste0(destinations$statefip, destinations$countyfip)

us_counties <- us_counties[mask, ]

us_counties@data <- us_counties@data %>% 
    left_join(y = destinations, 
              by = c("STATEFP" = "statefip", "COUNTYFP" = "countyfip")) %>% 
    left_join(us_states@data[, c("STATEFP", "STUSPS")], by = "STATEFP")

n_max <- round(max(us_counties@data$wt), digits = 0) + .5

bins <- seq(from = 0, to = n_max, by = n_max / 5)

pal <- colorBin("Blues", domain = us_counties@data$wt, bins = bins)

leaflet(data = us_counties) %>% 
    addProviderTiles(provider = "Stamen.Toner") %>%
    setView(
        zoom = 4, 
        lat = mean(coordinates(us_counties)[,2]), 
        lng = mean(coordinates(us_counties)[,1])
    ) %>% 
    addPolygons(
        fillColor = ~ pal(wt),
        popup = ~ paste0(
            "<b>", NAME, " (", STUSPS, ") :</b> ", 
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
        title = "Destinations", 
        labFormat = labelFormat(suffix = "%")
    ) %>% 
    addPolylines(
        data = us_states, 
        color = "black", 
        opacity = 1, 
        weight = 3
    )

# -- Mexico map: sources
mex_states <- readOGR('./shapefiles/mexstates/mexstates.shp')

mex_states@data <- mex_states@data %>% 
    mutate(GMI_ADMIN = str_sub(GMI_ADMIN, start = 5),
           CVE_ENT = str_extract(FIPS_ADMIN, pattern = "[[:digit:]]+"))

mex_municipios <- readOGR('./shapefiles/muni_2012gw/Muni_2012gw.shp', encoding = "UTF-8")

mask <- paste0(mex_municipios$CVE_ENT, mex_municipios$CVE_MUN) %in% paste0(sources$cve_ent, sources$cve_mun)

mex_municipios <- mex_municipios[mask, ]

mex_municipios@data <- mex_municipios@data %>% 
    left_join(y = sources, 
              by = c("CVE_ENT" = "cve_ent", "CVE_MUN" = "cve_mun")) %>% 
    left_join(mex_states@data[, c("CVE_ENT", "GMI_ADMIN")], by = "CVE_ENT")

n_max <- round(max(mex_municipios@data$wt), digits = 0) + .5

bins <- seq(from = 0, to = n_max, by = n_max / 5)

pal <- colorBin("Blues", domain = mex_municipios@data$wt, bins = bins)

leaflet(data = mex_municipios) %>% 
    addProviderTiles(provider = "Stamen.Toner") %>%
    setView(
        zoom = 4, 
        lat = mean(coordinates(mex_municipios)[,2]), 
        lng = mean(coordinates(mex_municipios)[,1])
    ) %>% 
    addPolygons(
        fillColor = ~ pal(wt),
        popup = ~ paste0(
            "<b>", NOM_MUN, " (", GMI_ADMIN, ") :</b> ", 
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
        title = "Destinations", 
        labFormat = labelFormat(suffix = "%")
    ) %>% 
    addPolylines(
        data = mex_states, 
        color = "black", 
        opacity = 1, 
        weight = 3
    )
