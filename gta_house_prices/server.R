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
library(xgboost)
# load district data
district_data <- read.csv('districts.csv')
# load model
model <- readRDS("xgboost_model.rds")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    inputData <- reactive({
        input$predict
        isolate(data.frame(
            'bathrooms'=as.integer(input$baths),
            'sqft'=as.integer(input$sqft),
            'parking'=as.integer(input$parking),
            'type'=as.factor(input$type),
            'mean_district_income'=as.integer(district_data$income[which(district_data$district==input$district)]),
            'bedrooms_total'=as.integer(input$beds)))
    })
    
    output$prediction <- renderText({
        input$predict
        if (input$predict == 0) {
            paste('Server is ready for calculation.')
            return()
        }
        data <- inputData()
        final_pred <- isolate(round(as.numeric(predict(model, newdata=data)), -3))
        list_pred <- isolate(round(as.numeric(-35372.83/0.9684345 + (1/0.9684345)*final_pred), -3))
        text <- isolate(local(paste(
            'The predicted list price is: $',
            list_pred,
            '. The predicted final price is: $',
            final_pred, '.')))
            
        isolate(local(HTML(paste(text))))
    })
    
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng=-79.39, lat=43.7, zoom= 5) %>%
            fitBounds(-79.6,43.59,-79.15,43.83) %>%
            addTiles() %>% addCircleMarkers(lng=district_data$long, lat=district_data$lat, label=(district_data$district), labelOptions(noHide=T), color='#2A81CB', radius = 10)
    })
    
    #change map focus
    district_change <- reactive({
        focus <- district_data %>% filter(district_data$district == input$district)
    })
    
    observe({
        district <- district_change()
        if (district$district == "Please select a district"){
            map <- leafletProxy("map")
            map %>% fitBounds(-79.6,43.59,-79.15,43.83) %>%
                addTiles() %>% addCircleMarkers(lng=district_data$long, lat=district_data$lat, label=(district_data$district), labelOptions(noHide=T), color='#2A81CB', radius = 10)
        } else {
            map <- leafletProxy("map")
            dist <- 0.02
            lat <- district$lat[1]
            lng <- district$long[1]
            nm <- district$district[1]
            map %>% fitBounds(lng-dist, lat-dist, lng+dist, lat+dist) %>%
                addTiles() %>% addCircleMarkers(lng=district_data$long, lat=district_data$lat, label=(district$district), labelOptions(noHide=T), color='#2A81CB', radius = 10)%>%
                addCircleMarkers(lng=lng, lat=lat, label=nm, labelOptions(noHide=T, textOnly = F), color='#9C2BCB', radius = 30)
        }
    })
})
