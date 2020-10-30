#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readr)
library(leaflet)
#read in the district data
district_data <- read.csv('districts.csv')
types <- list("Condo"=1, "Detached"=2, "Semi-Detached"=3, "Multiplex"=4, "Townhouse"=5)
districts <- as.list(district_data$district)
# Define UI for application that shows house prices
shinyUI(fluidPage(
    theme = shinytheme("superhero"),

    # Application title
    titlePanel("Toronto House Price Estimator"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("district",
                        "District",
                        districts,),
            radioButtons("type",
                        "Type",
                        types),
            numericInput("beds",
                        "Bedrooms",
                        min = 0,
                        max = 10,
                        value = 0),
            numericInput("baths",
                         "Bathrooms",
                         min = 0,
                         max = 10,
                         value = 0),
            numericInput("parking",
                         "Parking Spots",
                         min = 0,
                         max = 10,
                         value = 0),
            numericInput("sqft",
                         "Square Feet",
                         min = 200,
                         max = 4400,
                         step = 100,
                         value = 1000),
            actionButton('predict',
                         'Predict',
                         class='btn btn-primary')
        ),

        # Show the map
        mainPanel(
            leafletOutput("map"),
            textOutput("prediction")
        )
    )
))
