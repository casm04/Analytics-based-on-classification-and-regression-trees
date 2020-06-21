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
library(gapminder)
library(dplyr)


gapminder_1957 <- gapminder %>%
    filter(year==1957)
gapminder_2007 <- gapminder %>%
    filter(year==2007)


themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "dark" = theme_dark())

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("1957 vs 2007 life expectation comparison"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("g1957_bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 20), 
            
            sliderInput("g2007_bins",
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
        
        p1 <- ggplot(gapminder_1957, aes(x = lifeExp)) + 
            geom_histogram(bins = input$g1957_bins, color = colorline_displ, fill = colorfill_displ) +
            labs(x = 'Life expectation 1957', title = 'Life expectation 1957') +
            themes[[input$theme]]

        p2 <- ggplot(gapminder_2007, aes(x = lifeExp)) + 
            geom_histogram(bins = input$g2007_bins, color = colorline_cyl, fill = colorfill_cyl) +
            labs(x = "Life expectation 2007", title = "Life expectation 2007") +
            themes[[input$theme]]
        
        p1 + p2
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
