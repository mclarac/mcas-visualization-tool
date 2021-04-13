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
    # there are several counties/municipios with the same names
    # so we need to make sure which one is being selected in the dropdown lists
    mutate(county = paste0(statefip, ": ", county),
           nom_mun = paste0(cve_ent, ": ", nom_mun)) %>% 
    rename(statefp = statefip, countyfp = countyfip)

names(raw_data) <- names(raw_data) %>% toupper()

load(file = "shapefiles.RData")

# shapefiles transformations
mex_states@data <- mex_states@data %>%
    # remove MX for every GMI_ADMIN and extract only numbers from CVE_ENT
    # so variable values are consistent with raw_data
    mutate(STATEABBR = str_sub(GMI_ADMIN, start = 5),
           CVE_ENT = str_extract(FIPS_ADMIN, pattern = "[[:digit:]]+"))

mex_municipios@data <- mex_municipios@data %>% 
    rename(NAME = NOM_MUN)

us_states@data <- us_states@data %>% 
    rename(STATEABBR = STUSPS)

# -- helpers

# us_states_choices <- raw_data$STATE %>% unique() %>% sort()

us_states_tmp <- raw_data %>% 
    distinct(STATEFP, STATE) %>% 
    arrange(STATE)

us_states_choices <- us_states_tmp$STATEFP

names(us_states_choices) <- us_states_tmp$STATE

mex_states_tmp <- raw_data %>% 
    distinct(CVE_ENT, NOM_ENT) %>% 
    arrange(NOM_ENT)

mex_states_choices <- mex_states_tmp$CVE_ENT

names(mex_states_choices) <- mex_states_tmp$NOM_ENT

states_counties <- raw_data %>% 
    select(STATEFP, STATE, COUNTY) %>%
    distinct() %>% 
    mutate(ZONE = sample(
        x = 1:5, 
        size = nrow(.), 
        replace = TRUE
    )) %>% 
    arrange(COUNTY)

states_municipios <- raw_data %>% 
    select(CVE_ENT, NOM_ENT, NOM_MUN) %>%
    distinct() %>% 
    mutate(NOM_ZONE = sample(
        x = 1:5, 
        size = nrow(.), 
        replace = TRUE
    )) %>% 
    arrange(NOM_MUN)

# run the application
shinyApp(ui = ui, server = server)