#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# {r setup, include=FALSE}
library(shiny)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library (randomForest)
library(moderndive)
#library(tidyverse)
library(patchwork)
library(tidymodels)
library(modeldata)
library(baguette)
library(ROCR)
library(randomForest)
library(JOUSBoost)
library(caret)

knitr::opts_chunk$set(echo = TRUE)

# read the mushroom dataset
mushroom <- read.csv("C:\\Users\\cesarasa\\Documents\\CASM stuff\\Maestria Ciencia de Datos ITESO\\Verano2020\\Arboles de regresion\\Lecture final project\\datasets_478_974_mushrooms.csv")
mushroom$class[mushroom$class == 'p'] <- 'poisonous'
mushroom$class[mushroom$class == 'e'] <- 'edible'

mushroom_variables <- function(){
    mushroom_df <- data.frame(mushroom)
    return(colnames(mushroom_df)[-1])
}

catego_change <- function(df,y){
    df_encode <- data.frame(df)
    df_encode[y] <- as.factor(df[[y]])
    for (name_ in colnames(df_encode)[-1]){
        type_mush <- as.factor(c(df[[name_]]))
        df_encode[name_] <- unclass(type_mush)
    }
    return(df_encode)
}

mushroom_descrip  <- function(){
    # Attribute Information
    mush_features_lst <- list()
    mush_features_lst['cap.shape'] <- ' bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s'
    mush_features_lst['cap.surface'] <- ' fibrous=f,grooves=g,scaly=y,smooth=s'
    mush_features_lst['cap.color'] <- ' brown=n,buff=b,cinnamon=c,gray=g,green=r,pink=p,purple=u,red=e,white=w,yellow=y'
    mush_features_lst['bruises'] <- ' bruises=t,no=f'
    mush_features_lst['odor'] <- ' almond=a,anise=l,creosote=c,fishy=y,foul=f,musty=m,none=n,pungent=p,spicy=s'
    mush_features_lst['gill.attachment'] <- ' attached=a,descending=d,free=f,notched=n'
    mush_features_lst['gill.spacing'] <- ' close=c,crowded=w,distant=d'
    mush_features_lst['gill.size'] <- ' broad=b,narrow=n'
    mush_features_lst['gill.color'] <- ' black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e,white=w,yellow=y'
    mush_features_lst['stalk.shape'] <- ' enlarging=e,tapering=t'
    mush_features_lst['stalk.root'] <- ' bulbous=b,club=c,cup=u,equal=e,rhizomorphs=z,rooted=r,missing=?'
    mush_features_lst['stalk.surface.above.ring'] <- ' fibrous=f,scaly=y,silky=k,smooth=s'
    mush_features_lst['stalk.surface.below.ring'] <- ' fibrous=f,scaly=y,silky=k,smooth=s'
    mush_features_lst['stalk.color.above.ring'] <- ' brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y'
    mush_features_lst['stalk.color.below.ring'] <- ' brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y'
    mush_features_lst['veil.type'] <- ' partial=p,universal=u'
    mush_features_lst['veil.color'] <- ' brown=n,orange=o,white=w,yellow=y'
    mush_features_lst['ring.number'] <- ' none=n,one=o,two=t'
    mush_features_lst['ring.type'] <- ' cobwebby=c,evanescent=e,flaring=f,large=l,none=n,pendant=p,sheathing=s,zone=z'
    mush_features_lst['spore.print.color'] <- ' black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y'
    mush_features_lst['population'] <- ' abundant=a,clustered=c,numerous=n,scattered=s,several=v,solitary=y'
    mush_features_lst['habitat'] <- ' grasses=g,leaves=l,meadows=m,paths=p,urban=u,waste=w,woods=d'
    
    return(mush_features_lst)
}

tree_opt <- function(data, splt, y){
    # engine 
    engine <- c('C5.0','classification')
    
    # formula 
    form <- reformulate(termlabels = c('.'), response = c(y))
    # training split
    # train <- sample(1:nrow(data), nrow(data)*splt)
    # sample <- sample.int(n = nrow(data), size = floor(splt*nrow(data)), replace = F)
    train <- sample.int(n = nrow(data), size = floor(splt*nrow(data)), replace = F)
    # train <- data[sample, ]
    # test  <- data[-sample, ]
    # Bagger
    ctrl <- control_bag(var_imp = TRUE)
    bagg <- bagger(form, data = data[train,], base_model = engine[1])
    # boosted tree model 
    bt_model <-
        boost_tree(
            learn_rate = 0.3,
            trees = 5,
            tree_depth = 6,
            min_n = 1,
            sample_size = 1,
            mode = engine[2]
        ) %>% set_engine("xgboost", verbose = 2) %>%  fit(form, data = data[train,])
    # randomForest
    rf_model <-
        rand_forest(trees = 10, mtry = 5, mode = engine[2]) %>% 
        set_engine("randomForest",
                   # importance = T to have permutation score calculated
                   importance = T,
                   # localImp=T for randomForestExplainer(next post)
                   localImp = T,) %>% 
        fit(form, data = data[train,])
    # predictions 
    info.bag <- predict(bagg, data[-train,])
    info.bt <- predict(bt_model, data[-train,])
    info.rf <- predict(rf_model, data[-train,])
    # Metrics
    
    acc.bag <- confusionMatrix(info.bag$.pred_class, as.factor(data[-train,][[y]]))
    acc.bt <- confusionMatrix(info.bt$.pred_class, as.factor(data[-train,][[y]]))
    acc.rf <- confusionMatrix(info.rf$.pred_class, as.factor(data[-train,][[y]]))
    accuracy <- c(acc.bag$overall[1], acc.bt$overall[1], acc.rf$overall[1])
    
    # Result
    
    model <- c('Bagger', 'Boosted', 'RandomForest')
    data.frame(model, accuracy)
    #conf_matrix <- data.frame(Bagger=acc.bag, Boosted=acc.bt, RandomForest=acc.rf)
    
    return(list(Bagger=acc.bag, Boosted=acc.bt, RandomForest=acc.rf,
                Bag_mod=bagg, Boos_mod=bt_model, Rf_mod=rf_model))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("Mushrooms Classification by Decision Trees", style="color:firebrick", href="https://www.kaggle.com/kopylovlvad/mushrooms-classification-with-decision-tree"),
        tabPanel("Decision Tree Explanation",
                 fluidRow(
                     column(5, img(src = "decision_tree_2.png", hight=800, width = 700)),
                     column(3,
                            h2('What is a', strong('"Decision Tree"'),'?'), br(),
                            p("The decision trees are robust algorithms that built decision by spliting from Root Node to
                              sub nodes that get splited like ", strong("IF/ELSE"),"condiontional."), 
                            br(),
                            p("This work for Mushrooms will be for", strong('Classification')),
                            br(),
                            h3("The three types of Decision Three used are:"),
                            p("-Random Forest",br(),"-Bagging",br(),"-Boosting")
                            )
                 ),
                 br(),
                 code('by Mr. Oscuro'),
                 htmlOutput("link"),
        ),
        tabPanel("Mushroom Dataset analysis",
                 h2(a("Mushrooms Dataset", href="https://www.kaggle.com/kopylovlvad/mushrooms-classification-with-decision-tree")),
                 #h1("Favor de incluir el logo correspondiente a cada paquete elegido."),
                 uiOutput(outputId = 'logo'),
                 p("The Mushrooms dataset comes from", strong('kaggle'), "for classification proposes"),
                 p("The header of this page links you to:"),
                 p("https://www.kaggle.com/kopylovlvad/mushrooms-classification-with-decision-tree",style="color:blue"),
                 selectInput("mushroomvar", "Choose a comparison variable vs class of mushroom",
                             list('variable' = mushroom_variables()) # hacer una lista limpia con los paquetes principales
                 ),
                 p("Selection description:"),
                 verbatimTextOutput("txtout"),
                 tabsetPanel(
                    tabPanel("Mosaic plot analysis",
                        plotOutput('mosaic_plot')
                    ),
                    tabPanel("Distribution plot analysis",
                             plotOutput('bar_plot')
                    )
                 )
            ),
        tabPanel("Models",
                 tabsetPanel(
                    tabPanel("Random Forest",
                         fluidRow(
                             br(),
                             h3('The random forest approach is a bagging 
                                      method where deep trees, fitted on 
                                      bootstrap samples, are combined to produce 
                                      an output with lower variance.', style="color:firebrick"),
                             br(),
                             column(5,
                                    h2('Model Parameters'),
                                    verbatimTextOutput('rf_mod')
                                    ),
                             column(3,
                                    h2('Confusion Matrix'),
                                    br(),
                                    verbatimTextOutput("rf_conf_mtx")
                                    )
                         )
                        ),
                     tabPanel("Bagging",
                        fluidRow(
                            br(),
                            h3('The Bagging stands for bootstrap aggregating. It consists of building multiple 
                                 different decisin tree models from a single training data set by 
                                   repeatedly using multiple bootstraped subsets of the data and averaging 
                                   the models. Here, each tree is build independently to the others.',style="color:blue" ),
                            br(),
                          column(5,
                                 h2('Model Parameters'),
                                 verbatimTextOutput('bag_mod')
                          ),
                          column(3,
                                 h2('Confusion Matrix'),
                                 br(),
                                 verbatimTextOutput("bag_conf_mtx")
                          )
                        )
                     ),
                     tabPanel("Boosting",
                        fluidRow(
                            br(),
                            h3('Boosting is used to create a collection of predictors.
                              In this technique, learners are learned sequentially with early 
                              learners fitting simple models to the data and then analysing 
                              data for errors.',style="color:green"),
                            br(),
                          column(5,
                                 h2('Model Parameters'),
                                 verbatimTextOutput('boos_mod')
                                ),
                          column(3,
                                 h2('Confusion Matrix'),
                                 br(),
                                 verbatimTextOutput("bos_conf_mtx")
                          )
                        )
                     )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

mushroom_df <- data.frame(mushroom)
    output$txtout <- renderText({
        paste(input$mushroomvar,':',mushroom_descrip()[input$mushroomvar])
    })
    output$mosaic_plot <- renderPlot({
        ggplot(mushroom) + 
            geom_mosaic(aes(x = product(!!sym(input$mushroomvar)),fill=class)) +
            ggplot2::labs(x=input$mushroomvar,title=paste0('f(',input$mushroomvar,' | class)')) + 
            facet_grid(cols = vars(!!sym(input$mushroomvar))) +
            theme(strip.text.x=element_text(angle=0, hjust=0.5, vjust=0.5, size =15),
                  axis.text.y = element_text(angle=0, vjust=0.5, size = 15),
                  legend.text = element_text(size=15),
                  legend.title = element_text(color = "blue", size = 16)
                  )
    })
    output$bar_plot <- renderPlot({
        ggplot(mushroom,aes(!!sym(input$mushroomvar))) + 
            geom_bar(aes(fill=class), color='black', width=0.8) +
            theme(axis.text.x = element_text(angle=0, vjust=0.5, size = 15),
                  axis.text.y = element_text(angle=0, vjust=0.5, size = 15),
                  legend.text = element_text(size=15),
                  legend.title = element_text(color = "blue", size = 16)
                  ) +
            labs(title=paste("Mushroom edible and poison by ",input$mushroomvar),
                 subtitle='')
    })
    
    mushroom_df <- catego_change(data.frame(mushroom),'class')
    tree_ <- tree_opt(data = mushroom_df, splt = 0.80, y ='class')
    
    output$rf_conf_mtx <- renderPrint({
            as.table(tree_$RandomForest$table)
        })
    output$rf_mod <- renderPrint({
        tree_$Rf_mod$spec
    })
    output$bos_conf_mtx <- renderPrint({
        as.table(tree_$Boosted$table)
    })
    output$boos_mod <- renderPrint({
        tree_$Boos_mod$spec
    })
    output$bag_conf_mtx <- renderPrint({
        as.table(tree_$Bagger$table)
    })
    output$bag_mod <- renderPrint({
        tree_$Bag_mod$base_model
    })
    output$link <- renderUI({
        my_test <- tags$iframe(src='https://datascienceplus.com/mushrooms-classification-part-1/', height=600, width=600)
        print(my_test)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
