# --- GUI
ui <- shinyUI({
    
    fluidPage(
        
        titlePanel(title = "Migrant Destinations & Sources Distributions"),
        
        br(),
        
        # themeSelector(),
        
        theme = shinytheme("simplex"),
        
        sidebarLayout(
            
            sidebarPanel(
                
                width = 2,
                
                radioButtons(
                    inputId = "by", 
                    label = "Analyze by:", 
                    choices = c("Source", "Destination"), 
                    inline = TRUE
                ),
                
                hr(),
                
                uiOutput(outputId = "opts"),
                
                uiOutput(outputId = "opts2"),
                
                uiOutput(outputId = "opts3"),
                
                actionButton(
                    inputId = "go", 
                    label = "View Results"
                )
            ),
            
            mainPanel(
                width = 10,
                
                fluidRow(
                    column(
                        
                        width = 6, 
                        
                        # h4(textOutput(outputId = "title")),
                        
                        radioButtons(
                            inputId = "level", 
                            label = "Level of geographic aggregation:", 
                            choices = levels, 
                            inline = TRUE
                        ),
                        
                        leafletOutput(outputId = "map", height = "600px"),
                        
                        textOutput(outputId = "n_total", inline = TRUE)
                    ),
                    
                    column(
                        
                        width = 6,
                        
                        # h4(textOutput(outputId = "title2")),
                        
                        radioButtons(
                            inputId = "level2", 
                            label = "Level of geographic aggregation:", 
                            choices = levels, 
                            inline = TRUE
                        ),
                        
                        leafletOutput(outputId = "map2", height = "600px"),
                        
                        textOutput(outputId = "n_total2", inline = TRUE)
                    )
                )
            )
        )
    )
})