library("shiny")
library("bslib")
library("leaflet")

# --- GUI
ui <- shinyUI({
    
    fluidPage(
        
        titlePanel(title = "Migrant Destinations & Sources Distributions"),
        
        br(),
        
        theme = bs_theme(
            bg = "#ebebeb", 
            fg = "#000",
            primary = "#fcbf49", 
            secondary = "#003049", 
            base_font = font_google("PT Sans"), 
            heading_font = font_google("Bebas Neue"), 
            bootswatch = "litera"
        ),
        
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
                
                uiOutput(outputId = "opts3")
            ),
            
            mainPanel(
                width = 10,
                
                fluidRow(
                    column(
                        
                        width = 6, 
                        
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
                        
                        br(), br(),
                        
                        # textOutput(outputId = "aux", inline = TRUE), br(),
                        
                        downloadButton(
                            outputId = "downloadMap1", 
                            label = "Download Map",
                            icon = icon("map-marked-alt")
                        ),
                        
                        downloadButton(
                            outputId = "downloadData1", 
                            label = "Download Data",
                            icon = icon("database")
                        )
                    ),
                    
                    column(
                        
                        width = 6,
                        
                        uiOutput(outputId = "opts5"),
                        
                        leafletOutput(outputId = "map2", height = "600px"),
                        
                        textOutput(outputId = "n_total2", inline = TRUE),
                        
                        br(), br(),
                        
                        # textOutput(outputId = "aux2", inline = TRUE),
                        
                        downloadButton(
                            outputId = "downloadMap2", 
                            label = "Download Map",
                            icon = icon("map-marked-alt")
                        ),
                        
                        downloadButton(
                            outputId = "downloadData2", 
                            label = "Download Data",
                            icon = icon("database")
                        ),
                        
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