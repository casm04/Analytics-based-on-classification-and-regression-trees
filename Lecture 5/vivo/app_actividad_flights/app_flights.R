#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(moderndive)
library(colourpicker)
library(patchwork)
library(nycflights13)

flights_data <- flights
flights_data_filter <- flights_data %>%
    filter(dep_delay <= 180) %>%
    filter(dep_delay >= 5)
weather_data <- weather

themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "dark" = theme_dark())

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("NY airports characteristics Delays and temperatures"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Delay_bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 20), 
            
            sliderInput("Temp_bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 20), 
            
            colourInput("color", "Select Fill colour", value = "orange"),
            colourInput("colorLine", "Select Line colour", value = "white"),
            selectInput("theme", label = h4("Select theme for plot"), choices = names(themes))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        bins <- input$bins
        colorfill_displ <- input$color
        colorline_displ <- input$colorLine
        colorfill_cyl <- input$color
        colorline_cyl <- input$colorLine
        # draw the histogram with the specified number of bins
        
        p1 <- ggplot(flights_data_filter, aes(x = dep_delay)) + 
            geom_histogram(bins = input$Delay_bins, color = colorline_displ, fill = colorfill_displ) +
            labs(x = 'Departure Delay', title = 'Flight Departure Delays') +
            themes[[input$theme]]
        
        # p1 <- ggplot(planes, aes(type))
        # p1 + geom_density(aes(fill=factor(engines)), alpha=0.5) +
        #     labs(title="Density plot",
        #          subtitle= "", # "Engines per type of engine",
        #          caption="Source: flights_data",
        #          x="Airlines",
        #          fill="# Engines") +
        #     themes[[input$theme]]
        
        p2 <- ggplot(weather_data, aes(x = temp)) + 
            geom_histogram(bins = input$Temp_bins, color = colorline_cyl, fill = colorfill_cyl) +
            labs(x = "Temperature", title = "Temperature") +
            themes[[input$theme]]
        
        f <- flights_data %>%
            filter(dep_delay <= 180) %>%
            filter(dep_delay >= 10)
        
        p1 + p2
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
