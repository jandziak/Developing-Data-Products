library(ggplot2)
library(lattice)
library(caret)
library(ggplot2)
library(randomForest)
library(MASS)
library(e1071)
library(rpart)
data(GermanCredit)
GermanCredit <- GermanCredit[1:300,1:12]
inTrain <- createDataPartition(GermanCredit$Class, p = 0.7, list = F)
GermanCreditTest <- GermanCredit[-inTrain,]
GermanCredit <- GermanCredit[inTrain,]
set.seed(41242)
seeds <- vector(mode = "list", length = 41)
for(i in 1:40) seeds[[i]]<- sample.int(1000, 3)
seeds[[41]]<-sample.int(1000, 1)#for the last model
fitControl <- trainControl(## 10-fold CV
   method = "repeatedcv",
   number = 10,
   ## repeated ten times
   repeats = 4,
   seeds=seeds)

fit_glm <- train(Class ~ ., method='glm', data = GermanCredit, trControl = fitControl)
fit_rf <- train(Class ~., data = GermanCredit, method = "rf", trControl = trainControl(method = "none"), ntree = 50, tuneGrid = data.frame(.mtry = 4))
fit_lda <- train(Class ~ ., method='lda', data = GermanCredit, trControl = fitControl)

pr_glm <- predict(fit_glm, newdata = GermanCreditTest, type = "prob")
pr_glm <- pr_glm$Good
pr_glm <- data.frame(score = pr_glm, flag = ifelse(GermanCreditTest$Class == "Good", "Good", "Bad"))
pr_rf <- predict(fit_rf, newdata=GermanCreditTest, type="prob")
pr_rf <- pr_rf$Good
pr_rf <- data.frame(score = pr_rf, flag = ifelse(GermanCreditTest$Class == "Good", "Good", "Bad"))
pr_lda <- predict(fit_lda, newdata=GermanCreditTest, type="prob")
pr_lda <- pr_lda$Good
pr_lda <- data.frame(score = pr_lda, flag = ifelse(GermanCreditTest$Class == "Good", "Good", "Bad"))

shinyServer(function(input, output) {
   
   # Return the requested dataset
   datasetInput <- reactive({
      switch(input$dataset
             , "Random Forest" = fit_rf
             , "Logistic Regression" = fit_glm
             , "Linear Discriminant Analysis" = fit_lda)
   })
   pr <- reactive({
      switch(input$dataset
             , "Random Forest" = pr_rf
             , "Logistic Regression" = pr_glm
             , "Linear Discriminant Analysis" = pr_lda)
   })
   
   
   wykresInput <- reactive({
      switch(input$wykresy
             , "Histogram" = 1
             , "ROC Curve" = 2
             , "Distributions" = 3)
   })
   
   output$distPlot <- renderPlot({
      pr<-pr()
      x    <- pr$score[pr$flag=="Good"]  
      y    <- pr$score[pr$flag=="Bad"] 
      al <- input$alpha
      print(wykresInput())
      if(wykresInput() == 1)
      {
         ggplot(data=pr, aes(x=score, fill=flag)) + geom_density(alpha=al)
      }
      else if(wykresInput() == 2)
      {
         krok=0.01
         
         n <-length(pr$score[pr$flag=="Good"])
         m <-length(pr$score[pr$flag=="Bad"])
         progi<-seq(min(pr$score),max(pr$score),by=krok)
         frakcja.d<-frakcja.z<-rep(0,length(progi))
         for (i in 1 : length(progi))
         {
            pom1<-ifelse(pr$score<=progi[i],1,0)
            frakcja.d[i]=length(c(1:(m+n))[pom1==1 & pr$flag=="Good"])/n
            frakcja.z[i]=length(c(1:(m+n))[pom1==1 & pr$flag=="Bad"])/m
         }
         k <- length(frakcja.d)
         df<-data.frame(TPR = c(0,frakcja.d), FPR = c(0,frakcja.z))
         Area<-(df$TPR[2:(k+1)]-df$TPR[1:k])*(df$FPR[2:(k+1)]-df$FPR[1:k])/2+df$FPR[1:(k)]*(df$TPR[2:(k+1)]-df$TPR[1:k])
         AUROC<-sum(Area) 
         GINI<-2*AUROC-1
         
         
         ggplot( data=df, aes(x=TPR,y=FPR,color="Red" )) +  
            geom_line(alpha = 1,size=1.2) +
            labs(title = "ROC curve") +
            ylab("True Positive Rate (Sensitivity)") +
            xlab("False Positive Rate (1-Specificity)") + 
            theme(plot.title = element_text(lineheight=.8, face="bold")) + 
            geom_text(data = NULL, x = 0.75, y = 0.05, label = paste("AUROC =",round(AUROC,4)),color = "Black") +
            geom_text(data = NULL, x = 0.75, y = 0.15, label = paste("GINI =",round(GINI,4)),color = "Black") + ylim(0,1) + xlim(0,1)
      }
      else
      {
         krok=0.01
         
         n <-length(pr$score[pr$flag=="Good"])
         m <-length(pr$score[pr$flag=="Bad"])
         progi<-seq(min(pr$score),max(pr$score),by=krok)
         frakcja.d<-frakcja.z<-rep(0,length(progi))
         for (i in 1 : length(progi))
         {
            pom1<-ifelse(pr$score<=progi[i],1,0)
            frakcja.d[i]=length(c(1:(m+n))[pom1==1 & pr$flag=="Good"])/n
            frakcja.z[i]=length(c(1:(m+n))[pom1==1 & pr$flag=="Bad"])/m
         }
         k <- length(frakcja.d)
         df<-data.frame(x = c(0,progi,0,progi), yd = c(0,frakcja.d,0,frakcja.z)
                        ,lab = c(rep("F(s|GOOD)",k+1), rep("F(s|BAD)",k+1)))
         
         tab<-array(c(0,0,0,0),dim=c(length(progi),4))
         tab[,1]<-progi
         tab[,2]<-frakcja.d
         tab[,3]<-frakcja.z
         tab[,4]<-tab[,3]-tab[,2]
         KS<-max(tab[,4])
         ggplot( data=df, aes(x=x,y=yd,color=factor(lab) ))  +  
            geom_line(alpha = 1,size=1.2)+
            labs(title = "Wykres dystrybuant") +
            ylab("Prawdopodobienstwo") +
            xlab("x") + 
            theme(plot.title = element_text(lineheight=.8, face="bold"))+ 
            geom_text(data = NULL, x = 0.4, y = 0.05, label = paste("KS =",round(KS,4)),color = "Black")
         
      }
      
   })
   
   
   # Generate a summary of the dataset
   output$summary <- renderPrint({
      dataset <- datasetInput()
      summary(dataset)
   })
   
   
})