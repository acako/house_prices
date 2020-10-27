#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    inputData <- reactive({
        data.frame(
            "type"=input$type,
            "bedrooms"=input$beds,
            "bathrooms"=input,
            "parking"=input$parking,
            "sqft"=input$sqft,
            "district"=input$district)
    })

    output$map <- renderLeaflet({
        map = leaflet () %>% setview(lng = -79.39, lat=43.7) %>% fitBounds(-79.65,43.64,-79.15,43.9)
    })
})
