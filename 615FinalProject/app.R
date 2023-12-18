#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Map of Singapore"),
    
    leafletOutput("map"),
    
    dataTableOutput("KeyFacts"),
    
    textOutput("description")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  singapore_data <- st_layers("gadm41_SGP.gpkg")
  
  # 选择要读取的图层
  # layer_name <- "YOUR_LAYER_NAME"
  # singapore_data <- st_read("gadm41_SGP.gpkg", layer = layer_name)
  outpout$summary_output <- renderPrint({summary(singapore_data)})
  
  output$plot_output <- renderPlot({plot(singapore_data)})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
