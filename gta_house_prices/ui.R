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
#read in the district data
district_data <- read.csv('district_data.csv')

# Define UI for application that shows house prices
shinyUI(fluidPage(
    theme = shinytheme("superhero"),

    # Application title
    titlePanel("GTA House Price Estimator"),
    districts <- district_data$districts,
    types <- list("Condo", "Detached", "Semi-Detached", "Multiplex", "Townhouse"),
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
