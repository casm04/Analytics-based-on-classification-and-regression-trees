#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Titulo de la App"),
    sidebarLayout(
        position = 'right',
        sidebarPanel('sidebar panel'),
        mainPanel(
            h1('primer nivel',align = 'center'),
            h2('segundo nivel'),
            h3('tercel nivel'),
            p('Este funciona para hacer un parrafo nuevo...'),
                strong('strong () para hacer negrita la letra'),
                em('em() crea texto en italica'),
                    code('@cesar'),
            p('texto en color', style='color:red')
            )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
