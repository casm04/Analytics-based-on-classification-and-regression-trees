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
library(janeaustenr)
library(tidytext)
library(shinythemes)
book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE)

total_words <- book_words %>% 
    group_by(book) %>% 
    summarize(total = sum(n))


book_words <- left_join(book_words, total_words)

book_words

book_words <- book_words %>%
    bind_tf_idf(word, book, n)

book_words

book_words %>%
    select(-total) %>%
    arrange(desc(tf_idf))

# Define UI for application that draws a histogram
ui <- fluidPage(
    # shinythemes::themeSelector(),
    theme = shinytheme('simplex'),
    titlePanel('Analyzing word and document frequency'),
    sidebarLayout(
        sidebarPanel(
        p('A central question in text mining and natural language processing is hot to quantify 
                     what a document is about.'), 
        p('Can we do this by looking at the words that make up the
                     document?'),
        p('Here, as an example we will be considering'),
        h3("Jane Austen's Novels"),
        img(src='jane_austen.jpg',height=200,widht=160),
        br(),
        br(),
        code('by @oscuro')
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Goal',
                         br(),
                         br(),
                         h4('some options are:'),
                         p(strong('tf (term frequency) : '),
                           'How frequently a word occurs in a document'),
                         p('which decreases the weight for commonly used 
                           words and increases the weight for words that 
                           are not used very much in a collection of documents'),
                         p('The statistic',em("tf-idf"),'s intended to measure 
                           how important a word is to a document in a collection 
                           (or corpus) of documents'),
                         p('The inverse document frequency for any given term is defined as'),
                         #withMathJax('$$ idf(\\text{term})= \\ln{\\left(\\frac{n_{
                         #            \\text{documents containing}}}\\right)})} $$')
                         withMathJax("$$ idf(\\text{term})=
                                     \\ln{\\left(\\frac{n_{\\text{documents}}}{n_{\\text{documents containing term}}}\\right)}  $$")
                         ),
                tabPanel('Data',
                         h4('Summary'),
                         verbatimTextOutput('summary'),
                         fluidRow(
                            column(7,
                                   h4('Table'),
                                   tableOutput('table'),
                             ),
                            column(3,
                                   br(),
                                   br(),
                                   br(),
                                   p(strong('n'),'is the number of times that word is used in that book')
                            )
                         )
                    
                ),
                tabPanel('Term Frequency Plots',
                         plotOutput('plot_tf'),
                ),
                tabPanel("Zipf's law",
                         tableOutput("table_rank"),
                         
                         fluidRow(
                             column(8,plotOutput("plot_rank"),
                                    p("Zipf's law states that the frequency that a word
                                      appears is inversely proportional to its rank.")),
                             column(3,sliderInput("range_rank1",
                                                  "Rank's range:",
                                                  min = 1,
                                                  max = 10000,
                                                  value = c(10,1000)),
                                    withMathJax("$$\\text{frequency} \\propto \\frac{1}{\\text{rank}}$$")
                             )
                             
                         )
                         
                ),
                tabPanel("TF_IDF",
                         plotOutput('plot_tf_idf'),
                         sliderInput('n_words',
                                     "word's range:",
                                     min = 2,
                                     max = 50,
                                     value = 5
                                     )

                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$summary <- renderPrint({
        summary(austen_books())
    })
    output$table <- renderTable({
        head(book_words, 7)   
    })
    output$plot_tf <- renderPlot({
        ggplot(book_words, aes(n/total, fill = book, alpha = 0.5)) +
            scale_fill_brewer('Spectral') +
            geom_histogram(show.legend = FALSE) +
            xlim(NA, 0.0009) +
            facet_wrap(~book, ncol = 2, scales = "free_y")
    })
    
    output$table_rank <- renderTable({
        head(freq_by_rank,4)
    })
    
    rank_subset <- reactive({
        freq_by_rank %>%
            filter(rank < input$range_rank1[2],
                   rank > input$range_rank1[1])})
    
    output$plot_rank <- renderPlot({
        fitvals = lm(log10(`term frequency`) ~ log10(rank), data = rank_subset() )
        freq_by_rank %>%
            ggplot(aes(rank, `term frequency`, color = book)) +
            geom_line(size = 0.5, alpha = 0.8, show.legend = FALSE) +
            geom_abline(intercept = fitvals$coefficients[1],
                        slope = fitvals$coefficients[2], color = "gray50", linetype = 2) +
            geom_vline(xintercept = c(input$range_rank1[1],input$range_rank1[2]),
                       color = c("blue", "red")) +
            scale_x_log10() +
            scale_y_log10()
    })
    
    #word_subset <- reactive({
    #    freq_by_word %>%
    #        filter(word < input$word_rank1[2],
    #               word > input$word_rank1[1])})
    
#    word_subset <- book_words %>%
#        mutate(n_words)
    
    
    output$plot_tf_idf <- renderPlot({
        book_words %>%
            arrange(desc(tf_idf)) %>%
            mutate(word = factor(word, levels = rev(unique(word)))) %>% 
            group_by(book) %>% 
            # top_n(15) %>%
            top_n(input$n_words) %>% 
            ungroup() %>%
            
            ggplot(aes(word, tf_idf, fill = book)) +
            scale_fill_brewer('Spectral') +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "tf-idf") +
            facet_wrap(~book, ncol = 2, scales = "free") +
            coord_flip() + theme_minimal()
        #   theme(axis.text.x = element_text(size = 12),
            #  axis.text.y = element_text(size = 12 ),
            #  strip.text.x = element_text(size = 12, face = 'bold'),
            #  plot.title = element_text(hjust = 0.5 ,size = 40,family = 'Times'))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
