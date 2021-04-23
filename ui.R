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
                
                # actionButton(
                #     inputId = "go", 
                #     label = "View Results"
                # )
            ),
            
            mainPanel(
                width = 10,
                
                fluidRow(
                    column(
                        
                        width = 6, 
                        
                        # h4(textOutput(outputId = "title")),
                        
                        uiOutput(outputId = "opts4"),
                        
                        leafletOutput(outputId = "map", height = "600px"),
                        
                        textOutput(outputId = "n_total", inline = TRUE),
                        
                        tags$head(
                            tags$style(
                                "#n_total{ color: black;
                                 font-size: 16px;
                                 }"
                            )
                        ),
                        
                        br(),
                        
                        textOutput(outputId = "aux", inline = TRUE)
                    ),
                    
                    column(
                        
                        width = 6,
                        
                        # h4(textOutput(outputId = "title2")),
                        
                        uiOutput(outputId = "opts5"),
                        
                        leafletOutput(outputId = "map2", height = "600px"),
                        
                        textOutput(outputId = "n_total2", inline = TRUE),
                        
                        tags$head(
                            tags$style(
                                "#n_total2{ color: black;
                                 font-size: 16px;
                                 }"
                            )
                        )
                    )
                )
            )
        )
    )
})