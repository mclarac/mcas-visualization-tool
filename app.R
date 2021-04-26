library("shiny")
library("shinythemes")
library("bslib")
library("shinyauthr")
library("shinyjs")

library("tidyr")
library("dplyr")
library("stringr")
library("scales")
library("leaflet")
library("leaflet.extras")
library("mapview")

# --import shiny components
source(file = "ui.R", local = TRUE)
source(file = "server.R")

# -- import helper functions
source(file = 'utils.R')

# -- load cleaned and transformed data
source(file = 'cleaned_data.RData')

# -- load users and passwords
user_base <- readRDS("user_base.rds")

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
    arrange(NOM_ENT, NOM_MUN)

mex_frequencies <- get_frequencies(data = raw_data, country = "mex")

total_matriculas <- sum(raw_data$MATRICULA)

# -- input choices

us_states_choices <- states_counties$STATEFP %>% unique()

names(us_states_choices) <- states_counties$STATE %>% unique()

mex_states_choices <- states_municipios$CVE_ENT %>% unique()

names(mex_states_choices) <- states_municipios$NOM_ENT %>% unique()

# -- dummy input
# IMPORTANT: uncomment only for testing!!!
# input <- list(
#     by = "Destination",
#     us_state = "48",
#     # mex_state = mex_states_choices[1],
#     # municipio = municipios[1],
#     level = "State",
#     level2 = "State",
#     map_shape_click = list(id = "06")
# )

# run the application
shinyApp(ui = ui, server = server)