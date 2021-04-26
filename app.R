library("shiny")

# --import shiny components
source(file = "ui.R", local = TRUE)
source(file = "server.R", local = TRUE)

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