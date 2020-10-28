#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
# load district data
district_data <- read.csv('districts.csv')
# load model
model <- readRDS("decision_tree_model.rds")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    inputData <- reactive({
        data.frame(
            'bathrooms'=as.integer(input$baths),
            'sqft'=as.integer(input$sqft),
            'parking'=as.integer(input$parking),
            'type'=as.factor(input$type),
            'mean_district_income'=as.character(input$district),
            'beds'=as.integer(input$beds))
    })
    
    output$prediction <- renderText({
        data <- inputData()
        #data$mean_district_income[1] <- district_data$income[which(district_data$districts==data$income)]
        pred <- paste0(round(predict(model, newdata=data, type='vector')))
    })

    output$map <- renderLeaflet({
        leaflet () %>%
            setView(lng=-79.39, lat=43.7, zoom=5) %>%
            fitBounds(-79.65,43.64,-79.15,43.9) %>%
            addTiles() %>% addCircleMarkers(data=district_data, popup = ~as.character(district))
    })
})
