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
    titlePanel(
        strong("Page of Mr. Oscuro", align = 'center')
        ),
    sidebarLayout(
        position = 'right',
        sidebarPanel(
            h1('Mr. Oscuro', style = 'color:black'),
            p(img(src= 'eddie maiden and me.jpg', height = '80%', width = '90%')),
            p(strong('(AKA: Cesar A. Sanchez)')),
        ),
        mainPanel(
            h1(strong("Oscuro's programs"),align = 'left'),
            p(''),
            h2('First program'),
            #h3('tercel nivel'),
            #p('Este funciona para hacer un parrafo nuevo...'),
            #strong('strong () para hacer negrita la letra'),
            #em('em() crea texto en italica'),
            p(em('Here is to display the usage of shiny'), style='color:blue',size=20),
            p(code('@cesar')),
            p(em('print("Hello world!")'), style='color:black',size=16),
            # p('texto en color', style='color:red'),
            h3('Second program'),
            p(em('TBD')),
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
