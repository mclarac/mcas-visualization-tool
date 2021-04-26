# --- GUI
ui <- shinyUI({

    fluidPage(
        
        titlePanel(title = "Migrant Destinations & Sources Distributions"),
        
        # -- authentication
        # turn shinyjs on
        shinyjs::useShinyjs(),
        
        # add login panel UI function
        loginUI(id = "login"),
        
        # setup table output to show user info after login
        tableOutput("user_table"),
        
        br(),
        
        theme = bslib::bs_theme(
            bg = "#ebebeb", 
            fg = "#000",
            primary = "#fcbf49", 
            secondary = "#003049", 
            base_font = font_google("PT Sans"), 
            heading_font = font_google("Bebas Neue"), 
            bootswatch = "litera"
        ),
        
        # theme = shinytheme("simplex"),
        
        uiOutput(outputId = "sidebarlyt"),
        
        # add logout button UI 
        div(class = "pull-right", logoutUI(id = "logout"))
    )
    
})