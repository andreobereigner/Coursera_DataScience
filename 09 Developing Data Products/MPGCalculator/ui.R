library(shiny)
library(caret)

shinyUI (
  navbarPage("MPG Calculator",
             tabPanel("Application",
                fluidRow(
                  column(4,
                    p('It turns out that a vehicle\'s transmission, weight and horsepower are 
                    one of the key predictors when it comes to its MPG performance.'),
                    p('Therefore, you only need to adjust the follow parameters:'),
                    sliderInput('horsepower', 'Horsepower', value = 120, min = 50, max = 335, step = 5),
                    sliderInput('weight', 'Weight (lbs)', value = 3300, min = 1500, max = 5400, step = 100),
                    sliderInput('transmission', 'Transmission', value = 1, min = 0, max = 1, step = 1),
                    br(),
                    actionButton('goButton', 'Calculate!')
                    ),
                  column(8,
                    fluidRow(
                      column(8,
                         h4('Results'),
                         p('Your vehicle\'s predicted MPG performance is:'),
                         strong(textOutput('predictionMPG')),
                         span(textOutput('predictionKMPL')),
                         br(),
                         p('In the following graph, you will see your parameter selections
                           compared to the observed data in the mtcars data set.')
                        )   
                      ),
                    fluidRow(
                      column(4,
                             plotOutput('graph01')
                      ),
                      column(4,
                             plotOutput('graph02')
                      )
                    )
                  )
                )
                
                
              ),
             
           tabPanel("About",
                    mainPanel(
                      includeMarkdown("includeAbout.md")
                    )
           )
  )
)