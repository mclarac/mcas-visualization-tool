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

# --import shiny components
source(file = "ui.R", local = TRUE)
source(file = "server.R")

# -- data manipulation functions
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
            select(CVE_ENT, CVE_CZ, GEOID_MX, MATRICULA) %>% 
            gather(LEVEL, GEOID, -MATRICULA) %>% 
            mutate(LEVEL = plyr::mapvalues(
                x = LEVEL, 
                from = c("CVE_ENT", "CVE_CZ", "GEOID_MX"), 
                to = levels)
            )
            
    }
    
    frequencies <- frequencies %>% 
        group_by(LEVEL, GEOID) %>% 
        summarise(n_total = sum(MATRICULA), .groups = "drop") %>% 
        group_by(LEVEL) %>% 
        mutate(wt = n_total / sum(n_total) * 100)
    
    return(frequencies)
}

filter_data <- function(data, ids, by = "Source"){
    
    # states <- sapply(ids, FUN = function(x) str_sub(x, end = 2)) %>% unique()
    
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

# to check data inconsistencies
# aux <- commuting_zones %>% 
#     distinct(cz, statename, cz_name)
# 
# aux_counts <- aux %>% count(statename, cz_name, sort = TRUE)

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

# tmp <- filter(raw_data, is.na(CZ_NAME)) %>% distinct(GEOID_US, STATE, COUNTY)

# -- shapefiles transformations
load(file = "shapefiles.RData")

mex_states@data <- mex_states@data %>%
    # remove MX for every GMI_ADMIN and extract only numbers from CVE_ENT
    # so variable values are consistent with raw_data
    mutate(STATEABBR = str_sub(GMI_ADMIN, start = 5),
           GEOID = str_extract(FIPS_ADMIN, pattern = "[[:digit:]]+"))

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

# -- helpers

levels <- c("State", "Commuting zone", "County/Municipio")

states_counties <- raw_data %>% 
    select(GEOID_US, STATEFP, STATE, CZ, CZ_NAME, COUNTYFP, COUNTY) %>%
    distinct() %>% 
    arrange(STATE, CZ_NAME, COUNTY)

us_frequencies <- get_frequencies(data = raw_data)

states_municipios <- raw_data %>% 
    select(GEOID_MX, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) %>%
    distinct() %>% 
    # include dummy columns for mexican commuting zones 
    mutate(
        CVE_CZ = sample(
            x = 1:5, 
            size = nrow(.), 
            replace = TRUE
        ),
        NOM_CZ = plyr::mapvalues(
            x = CVE_CZ, 
            from = 1:5, 
            to = c("a", "b", "c", "d", "e")
        )) %>% 
    arrange(NOM_ENT, NOM_CZ, NOM_MUN)

raw_data <- raw_data %>% 
    # NOTE: this is temporary
    left_join(states_municipios %>% select(GEOID_MX, CVE_CZ, NOM_CZ),
              by = "GEOID_MX") 

mex_frequencies <- get_frequencies(data = raw_data, country = "mex")

total_matriculas <- sum(raw_data$MATRICULA)

# -- input choices

us_states_choices <- states_counties$STATEFP %>% unique()

names(us_states_choices) <- states_counties$STATE %>% unique()

mex_states_choices <- states_municipios$CVE_ENT %>% unique()

names(mex_states_choices) <- states_municipios$NOM_ENT %>% unique()

# -- dummy input
# IMPORTANT: uncomment only for testing!!!
input <- list(
    by = "Destination", 
    us_state = us_states_choices[1]
    # mex_state = mex_states_choices[1],
    # municipio = municipios
)

# run the application
shinyApp(ui = ui, server = server)