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
library(caret)
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
            'typeCondo'=as.integer(ifelse(input$type==1,1,0)),
            'typeDetached'=as.integer(ifelse(input$type==2,1,0)),
            'typePlex'=as.integer(ifelse(input$type==3,1,0)),
            'typeSemiDetached'=as.integer(ifelse(input$type==4,1,0)),
            'typeTownhouse'=as.integer(ifelse(input$type==5,1,0)),
            'mean_district_income'=as.integer(district_data$income[which(district_data$district==input$district)]),
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
