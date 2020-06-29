
# https://www.tidymodels.org/
#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
library(shiny)
library(tidymodels)
library(htm2txt)

d_broom <- "broom summarizes key information about models in tidy tibble()s. 
broom provides three verbs to make it convenient to 
interact with model objects:
- tidy() summarizes information about model components
- glance() reports information about the entire model
- augment() adds informations about observations to a dataset
For a detailed introduction, please see vignette(\"broom\").

broom tidies 100+ models from popular modelling packages and almost all of the model
objects in the stats package that comes with base R. vignette(\"available-methods\")
lists method availability.

If you aren’t familiar with tidy data structures and want to know how they
can make your life easier, we highly recommend reading Hadley Wickham’s Tidy Data.
"

d_rsample <- "rsample contains a set of functions to create different types of resamples 
and corresponding classes for their analysis. The goal is to have a modular set of methods 
that can be used across different R packages for:

traditional resampling techniques for estimating the sampling distribution of a statistic and
estimating model performance using a holdout set
The scope of rsample is to provide the basic building blocks for creating and analyzing 
resamples of a data set but does not include code for modeling or calculating statistics. 
The 'Working with Resample Sets' vignette gives demonstrations of how rsample tools can be used.

Note that resampled data sets created by rsample are directly accessible in a resampling 
object but do not contain much overhead in memory. Since the original data is not modified, 
R does not make an automatic copy.
"

#overviews_keys <- c("broom",'rsample')
#overviews_descriptions <- c(d_broom,d_rsample)
#names(overviews_descriptions) <- overviews_keys

url <- 'https://parsnip.tidymodels.org/'
d_parsnip <- gettxt(url)

overviews_keys <- list("broom",'rsample','parsnip')
overviews_descriptions <- list(d_broom,d_rsample,d_parsnip)
names(overviews_descriptions) <- overviews_keys

# Define UI for application
ui <- fluidPage(
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("Tidymodels", style="color:firebrick", href="https://www.tidymodels.org/"),
        tabPanel("Installation",
                 fluidRow(
                     column(5, img(src = "tidymodels.png", hight=300, width = 300)),
                     column(3,
                            h3("TIDYMODELS"), br(),
                            p("The tidymodels framework is a collection of packages for modeling 
                            and machine learning using", strong("tidyverse"),"principles."), 
                            p("Install tidymodels with:"),
                            br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            p("Run", em("library(tidymodels)"), "to load the core packages and make 
                              them available in your current R session"))
                 ),
                 br(),
                 code('by Mr. Oscuro'),
                ),
        tabPanel("Packages", 
                 h3("CORE TIDYMODELS"),
                 #h1("Favor de incluir el logo correspondiente a cada paquete elegido."),
                 uiOutput(outputId = 'logo'),
                 p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
                 selectInput("state", "Choose a tidymodel library:",
                             list('package' = tidymodels_packages()) # hacer una lista limpia con los paquetes principales
                             ),
                 # verbatimTextOutput("result")
                 # htmlOutput("result")
                 # verbatimTextOutput("frame")
                 htmlOutput("frame")
                 # htmlOutput("frame",list('package' = tidymodels_packages()))
                 
                 
                 ),
        tabPanel("Learn", # "This panel is intentionally left blank",
                     tabsetPanel(
                         # tabPanel("Cathegory 1",
                         #         h4("Table"),
                         #         tableOutput("table"),
                         #         h4("Verbatim text output"),
                         #         verbatimTextOutput("txtout"),
                         #         h1("Header 1"),
                         #         h2("Header 2"),
                         #         h3("Header 3"),
                         #         h4("Header 4"),
                         #         h5("Header 5")
                         # ),
                         tabPanel("PERFORM STATISTICAL ANALYSIS",
                            tabsetPanel(
                                tabPanel('Correlation and regression fundamentals with
                                         tidy data principles',
                                    h5('Analyze the results of correlation tests and
                                     simple regression models for many data sets at once.')
                                         ),
                                tabPanel("K-means clustering with tidy data principles",
                                    h5('Summarize clustering characteristics and
                                    estimate the best number of clusters for a data set.')
                                         ),
                                tabPanel("Bootstrap resampling and tidy regression models",
                                     h5('Apply bootstrap resampling to estimate uncertainty
                                        in model parameters.')
                                        ),
                                tabPanel("Hypothesis testing using resampling and tidy data",
                                     h5('Perform common hypothesis tests for statistical
                                        inference using flexible functions.')
                                        ),
                                tabPanel("Statistical analysis of contingency tables",
                                     h5('Use tests of independence and goodness of
                                        fit to analyze tables of counts.')
                                        )
                                )
                         ),
                        tabPanel("CREATE ROBUST MODELS", 
                            tabsetPanel(
                                tabPanel('Regression models two ways',
                                    h5('Create and train different kinds of 
                                       regression models with different computational engines')
                                        ),
                                tabPanel('Classification models using a neural network',
                                         h5('Train a classification model and evaluate 
                                            its performance')
                                        )
                                    )
                                ),
                        tabPanel("Tab 3", "This panel is intentionally left blank")
                        )
                    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
        head(cars, 4)
    })
    # output$result <- renderText({
    #     paste(overviews_descriptions[input$state])
    #     #HTML(overviews_descriptions[input$state])
    # })
    output$frame <- renderUI({
        my_test <- tags$iframe(src=paste0("https://", input$pkg_name,".tidymodels.org"), height=600, width=1500)
        my_test <- tags$iframe(src=paste0("https://", input$state,".tidymodels.org"), height=600, width=1200)
        print(my_test)
        # tags$iframe(src=paste0("https://", input$state,".tidymodels.org"), height=600, width=1200)
    })
    image_url <- 'https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/'
    output$logo <- renderUI(
        # tags$img(src='https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/tune.png',height=200,widht=160)
        tags$img(src=paste0("./PNG/", input$state, ".png"),height=100,widht=100)
        # tags$img(src=paste0(image_url, input$state, ".png"),height=100,widht=100)
    )
}

# Run the application 
shinyApp(ui, server)
