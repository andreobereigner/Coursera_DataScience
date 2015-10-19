#setwd("C:/Users/andre/Documents/_DATA/Career/_C_DataScienceCertification/09_DevelopingDataProducts/CourseProject/MPGCalculator")

#devtools::install_github("hrbrmstr/metricsgraphics")
#install.packages("htmlwidgets")
#install.packages("shinyapps)
#install.packages("RcppEigen")

library(shiny)
library(caret)
library(ggplot2)
library(markdown)


set.seed(100)

data(mtcars)

mtcarsOriginal <- mtcars
mtcarsAdj <- mtcars

mtcarsAdj$cyl <- as.factor(mtcarsOriginal$cyl)
mtcarsAdj$vs <- as.factor(mtcarsOriginal$vs)
mtcarsAdj$am <- factor(mtcarsOriginal$am)
mtcarsAdj$gear <- factor(mtcarsOriginal$gear)
mtcarsAdj$carb <- factor(mtcarsOriginal$carb)

# Stepwise variable selection with "glmStepAIC".
# model01 <- train(mpg ~ .,
#                  data = mtcarsAdj,
#                  method = "glmStepAIC",
#                  metric = "RMSE")
# model01
# summary(model01)

finalModel <- train(mpg ~ hp + wt + am,
                 data = mtcarsAdj,
                 method = "glm",
                 metric = "RMSE")
#finalModel
#summary(finalModel)

shinyServer(
  function(input, output) { 
      
    output$predictionMPG <- renderText({
      input$goButton
      #if (input$goButton == 0)
      #  return()
      
      isolate({
        predictedMPG <- predict(finalModel, 
                                data.frame(hp = input$horsepower,
                                            wt = input$weight / 1000, 
                                            am = as.factor(input$transmission)))
        paste(round(predictedMPG, 2), "Miles per Gallon")
      })
      
    })
    
    output$predictionKMPL <- renderText({
      input$goButton
      #if (input$goButton == 0)
      #  return()
      
      isolate({
        predictedMPG <- predict(finalModel, 
                                data.frame(hp = input$horsepower,
                                           wt = input$weight / 1000, 
                                           am = as.factor(input$transmission)))
        paste(round(predictedMPG * 0.425144, 2), "Kilometers per Liter")
      })
      
    })
    
    output$graph01 <- renderPlot({
      input$goButton
      #if (input$goButton == 0)
      #  return()
      
#       plot(mtcarsAdj$hp, mtcarsAdj$mpg,
#            ylab='Miles/(US) Gallon', xlab='Gross Horsepower', 
#            col='lightblue', main='Observed MPG by Horsepower')
#       horsepower <- input$horsepower
#       lines(c(horsepower, horsepower), c(0, 200),col="red",lwd=1)
#       text(63, 150, paste("horsepower = ", horsepower))

      hist(mtcarsAdj$hp, xlab='Gross Horsepower', 
           col='lightgrey', main='Horsepower Histogram')
      horsepower <- input$horsepower
      lines(c(horsepower, horsepower), c(0, 200), col="red", lwd=1)
      
      
    })
    
    output$graph02 <- renderPlot({
      input$goButton
      #if (input$goButton == 0)
      # return()
      
#       plot(mtcarsAdj$wt, mtcarsAdj$mpg,
#            ylab='Miles/(US) Gallon', xlab='Weight (lb/1000)', 
#            col='lightblue', main='Observed MPG by Weight')
#       weight <- input$weight / 1000
#       lines(c(weight, weight), c(0, 200),col="red",lwd=1)
#       text(63, 150, paste("weight = ", weight))
      
      hist(mtcarsAdj$wt, xlab='Weight (lbs/1000)', 
           col='lightgrey', main='Weight Histogram')
      weight <- input$weight / 1000
      lines(c(weight, weight), c(0, 200), col="red",lwd=1)
      
      isolate({
        
      })
      
    })
    
  }
)
