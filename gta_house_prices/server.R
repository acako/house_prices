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
        if (input$predict == 0) {
            paste('Server is ready for calculation.')
            return()
        }
        data <- inputData()
        list_pred <- isolate(round(as.numeric(predict(model, newdata=data, type='vector')), -3))
        final_pred <- isolate(round(as.numeric(35372.83 + 0.9684345*list_pred), -3))
        text <- isolate(local(paste(
            'The predicted list price is: $',
            list_pred,
            '. The predicted final price is: $',
            final_pred)))
            
        isolate(local(HTML(paste(text))))
    })

    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng=-79.39, lat=43.7, zoom= 5) %>%
            fitBounds(-79.6,43.59,-79.15,43.83) %>%
            addTiles() %>% addCircleMarkers(data=district_data, popup = ~as.character(district))
    })
    
    #change map focus
    district_change <- reactive({
        focus <- district_data %>% filter(district_data$district == input$district)
    })

    observeEvent(input$district,{
        district <- district_change()
        if (nrow(district) > 0){
            map <- leafletProxy("map")
            dist <- 0.02
            lat <- district$lat[1]
            lng <- district$long[1]
            nm <- district$district[1]
            map %>% fitBounds(lng-dist, lat-dist, lng+dist, lat+dist) %>%
                addTiles() %>% addCircleMarkers(data=district_data, popup = ~as.character(district))
        }
    })
})
