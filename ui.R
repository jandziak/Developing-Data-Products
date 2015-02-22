shinyUI(pageWithSidebar(
   headerPanel("Interactive modeler for predicting credit score"),
   sidebarPanel(
      selectInput("dataset", "Wybierz model:", 
                  choices = c("Logistic Regression", "Random Forest", "Linear Discriminant Analysis")),
      
      selectInput("wykresy", "Wybierz dane:", 
                  choices = c("Histogram", "ROC Curve", "Distributions")),
      
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      submitButton("Refresh"),
      
      sliderInput("alpha",
                  "Parameter alpha:",
                  min = 0,
                  max = 1,
                  value = 0.7)
   ),
   mainPanel(
      h3("Statistics"),
      plotOutput("distPlot"),      
      h3("Functionality"),
      h5("This aaplication is an modeler to solve prediction task. 
         Using German Credit data we use 12 variables
         to predict behavviour of clients. "),
      h5("There are 3 models fited: Logistoic Regression Random Forests and Linear Discriminant Analysis. 
         For all of them there are some statistics calculated KS statisitic AUROC. 
         Using Swich 1 you can choice model and then using second swich you
         can choice which statistic do you want to see."),
      h5("This is reactive application so to change anything it you have to push \"Refresh\" buton"),
      h5("There is also slider to change parameter alpha for density plot"),
      h5("Remember to use refresh button each time yuo want to see changes"),
      h5("This application requires packages\"caret\" and \"ggplot2\"" )
      
   )
))