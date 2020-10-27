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
district_data <- read.csv('district_data.csv')
types <- list("Condo", "Detached", "Semi-Detached", "Multiplex", "Townhouse")
districts <- as.list(district_data$districts)
# Define UI for application that shows house prices
shinyUI(fluidPage(
    theme = shinytheme("superhero"),

    # Application title
    titlePanel("GTA House Price Estimator"),
    leaflet('map', width='100%', height='100%'),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("type",
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
            selectInput("district",
                        "District",
                        districts)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
