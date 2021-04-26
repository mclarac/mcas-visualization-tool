library("tidyr")
library("dplyr")
library("stringr")
library("readr")
library("rgdal")
library("sf")

# -- data loading
mainfile <- './mcas_2008.csv'

us_cz_file <- './shapefiles/cz1990_shapefile/cty_cz_st_crosswalk.csv'

raw_data <- read_csv(
    file = mainfile, 
    col_types = cols(
        nom_mun = col_character(),
        nom_ent = col_character(),
        county = col_character(),
        state = col_character(),
        .default = col_double()
    )
)

commuting_zones <- read_csv(
    file = us_cz_file,
    col_types = cols(
        .default = col_character()
    )
)

commuting_zones <- commuting_zones %>% 
    mutate_at(.vars = c("cty", "cz"),
              .funs = function(x) str_pad(x, width = 5, pad = "0")) %>% 
    select(geoid_us = cty, cz, cz_name)

raw_data <- raw_data %>% 
    mutate_at(.vars = c("statefip", "cve_ent"),
              .funs = function(x) str_pad(x, width = 2, pad = "0")) %>% 
    mutate_at(.vars = c("countyfip", "cve_mun"),
              .funs = function(x) str_pad(x, width = 3, pad = "0"),) %>% 
    mutate(geoid_us = paste0(statefip, countyfip),
           geoid_mx = paste0(cve_ent, cve_mun)) %>% 
    left_join(commuting_zones, by = "geoid_us") %>% 
    # there are several counties/municipios with the same names
    # so we need to make sure which one is being selected in the dropdown lists
    mutate(county = paste0(statefip, ": ", county),
           nom_mun = paste0(cve_ent, ": ", nom_mun)) %>% 
    rename(statefp = statefip, countyfp = countyfip)

names(raw_data) <- names(raw_data) %>% toupper()

# -- shapefiles transformations
us_states <- readOGR('./shapefiles/cb_2016_us_state_5m/cb_2016_us_state_5m.shp')

us_counties <- readOGR('./shapefiles/cb_2016_us_county_5m/cb_2016_us_county_5m.shp')

us_cz <- readOGR('./shapefiles/cz1990_shapefile/cz1990.shp')

mex_states <- readOGR('./shapefiles/mexstates/mexstates.shp')

mex_municipios <- readOGR('./shapefiles/muni_2012gw/Muni_2012gw.shp', encoding = "UTF-8")

mex_states@data <- mex_states@data %>%
    # remove MX for every GMI_ADMIN and extract only numbers from CVE_ENT
    # so variable values are consistent with raw_data
    mutate(STATEABBR = str_sub(GMI_ADMIN, start = 5),
           GEOID = str_extract(FIPS_ADMIN, pattern = "[[:digit:]]+")) %>% 
    rename(NAME = ADMIN_NAME)

mex_municipios@data <- mex_municipios@data %>% 
    rename(NAME = NOM_MUN) %>% 
    mutate(GEOID = paste0(CVE_ENT, CVE_MUN)) %>% 
    left_join(y = mex_states@data[, c("GEOID", "STATEABBR")], 
              by = c("CVE_ENT" = "GEOID"))

us_states@data <- us_states@data %>% 
    rename(STATEABBR = STUSPS)

us_counties@data <- us_counties@data %>% 
    left_join(y = us_states@data[, c("STATEFP", "STATEABBR")],
              by = "STATEFP")

us_cz <- readOGR('./shapefiles/cz1990_shapefile/cz1990.shp')

commuting_zones <- commuting_zones %>% 
    left_join(us_counties@data %>% select(GEOID, STATEABBR),
              by = c('geoid_us' = 'GEOID')) %>% 
    select(-geoid_us) %>% 
    # TODO: check why when keeping distinct columns considering all cols, the # of CZs increases
    distinct(cz, .keep_all = TRUE)

us_cz@data <- us_cz@data %>% 
    mutate(cz = str_pad(cz, width = 5, pad = "0")) %>% 
    left_join(commuting_zones, by = "cz") %>% 
    rename(GEOID = cz, NAME = cz_name)

save.image(file = "cleaned_data.RData")
