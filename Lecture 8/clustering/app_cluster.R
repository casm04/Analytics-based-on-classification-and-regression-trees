#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidymodels)
library(tidyverse)
library(imager)

ui <- fluidPage(
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("K-means clustering with tidy data principles", style="color:firebrick", 
          href="https://www.tidymodels.org/learn/statistics/k-means/"),
        tabPanel("LEARNING OBJECTIVE",
                 fluidRow(
                     # column(5, img(src = "tidymodels.png", hight=300, width = 300)),
                     column(5, img(src = "https://www.tidymodels.org/learn/statistics/k-means/kmeans.gif", hight=300, width = 300)),
                     column(3,
                            h3("K-Means"), br(),
                            p("Summarize clustering characteristics and estimate 
                              the best number of clusters for a data set.") 
                            # p("Install tidymodels with:"),
                            # br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            # p("Run", em("library(tidymodels)"), "to load the core packages and make 
                            #   them available in your current R session"))
                 ),
                 br(),
                 code('by Mr. Oscuro'),
        )
        # tabPanel("INTRODUCTION", 
        #          h3("CORE TIDYMODELS"),
        #          #h1("Favor de incluir el logo correspondiente a cada paquete elegido."),
        #          uiOutput(outputId = 'logo'),
        #          p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
        #          selectInput("state", "Choose a tidymodel library:",
        #                      list('package' = tidymodels_packages()) # hacer una lista limpia con los paquetes principales
        #          ),
        #          # verbatimTextOutput("result")
        #          # htmlOutput("result")
        #          # verbatimTextOutput("frame")
        #          htmlOutput("frame")
        #          # htmlOutput("frame",list('package' = tidymodels_packages()))
        #          
        #          
        #         ),
        # tabPanel("Learn", # "This panel is intentionally left blank",
        #          tabsetPanel(
        #              # tabPanel("Cathegory 1",
        #              #         h4("Table"),
        #              #         tableOutput("table"),
        #              #         h4("Verbatim text output"),
        #              #         verbatimTextOutput("txtout"),
        #              #         h1("Header 1"),
        #              #         h2("Header 2"),
        #              #         h3("Header 3"),
        #              #         h4("Header 4"),
        #              #         h5("Header 5")
        #              # ),
        #              tabPanel("PERFORM STATISTICAL ANALYSIS",
        #                       tabsetPanel(
        #                           tabPanel('Correlation and regression fundamentals with
        #                                  tidy data principles',
        #                                    h5('Analyze the results of correlation tests and
        #                              simple regression models for many data sets at once.')
        #                           ),
        #                           tabPanel("K-means clustering with tidy data principles",
        #                                    h5('Summarize clustering characteristics and
        #                             estimate the best number of clusters for a data set.')
        #                           ),
        #                           tabPanel("Bootstrap resampling and tidy regression models",
        #                                    h5('Apply bootstrap resampling to estimate uncertainty
        #                                 in model parameters.')
        #                           ),
        #                           tabPanel("Hypothesis testing using resampling and tidy data",
        #                                    h5('Perform common hypothesis tests for statistical
        #                                 inference using flexible functions.')
        #                           ),
        #                           tabPanel("Statistical analysis of contingency tables",
        #                                    h5('Use tests of independence and goodness of
        #                                 fit to analyze tables of counts.')
        #                           )
        #                       )
        #              ),
        #              tabPanel("CREATE ROBUST MODELS", 
        #                       tabsetPanel(
        #                           tabPanel('Regression models two ways',
        #                                    h5('Create and train different kinds of 
        #                                regression models with different computational engines')
        #                           ),
        #                           tabPanel('Classification models using a neural network',
        #                                    h5('Train a classification model and evaluate 
        #                                     its performance')
        #                           )
        #                       )
        #              ),
        #              tabPanel("Tab 3", "This panel is intentionally left blank")
        #          )
        #      )
          )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
