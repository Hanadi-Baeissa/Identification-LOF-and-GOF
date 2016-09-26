set.seed(12345)
library(plyr)
library(randomForest)
data <- read.csv("OGvTS.csv",header=TRUE,row.names = 1) # Read Data 
colnames(data)[1] <- "Class"  # assign response class 
# split data in to predictors and response
dataX <- data[,2:length(data)] 
dataY <- as.factor(data[,1])

# handle missing values to median. Can also use mean , mode 
impute.value <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
data.new <- sapply(dataX, function(x){
    if(is.numeric(x)){
            impute.value(x)
        } else {
            x
        }
    }
)
 dataX <- as.data.frame(data.new) # create data frame
 
 
 #!MODEL WITH CROSS-VALIDATION!#

data <- cbind(dataX,dataY)
colnames(data)[length(data)] <- "Class"
  
## Using custom code

K1 = 10 # set number of folds  
n = floor(nrow(dataX)/K1)  # difines size of fold
error.vector <- rep(NA,K1)
accuracies <-c()
acc_old <- 0.0
for (i in 1:K1){
		rr1 <- ((i -1) * n + 1) # start of subset
		rr2 <- (i * n) # end of subset 
		subset.range <- rr1 : rr2 # difine range .Though subsets are not picked randomly it may show poor performance. Better result required resampling without repetition
		train.X <- data[-subset.range,(1:length(data) -1)]
		train.Y <- data[-subset.range,length(data)]
		train.Y <-  revalue(train.Y,c("OG" = "1","TS" = "0"))
		test.X <- data[subset.range,(1:length(data)-1)]
		test.Y <- data[subset.range,length(data)]
		test.Y <-  revalue(test.Y,c("OG" = "1","TS" = "0"))
		
		modelFit <- randomForest(x=train.X,y=as.factor(train.Y),ntree=100, importance=TRUE,nodesize=5)  # depth = 5 no. of tree = 10. change accordingly. 
		predict.test <- predict(modelFit,newdata=test.X,type = "response")
		

       test.X$rightPred <- predict.test == test.Y
       t<-table(predict.test, test.X$rightPred)
       
       accuracy <- sum(test.X$rightPred)/nrow(test.X)
       accuracies <- c(accuracies,accuracy)
      ## save model which has higher accuracy 
       if (accuracy >= acc_old) {
       save(modelFit,file="model_custom.RData")
       acc_old <- accuracy
       #print(acc_old)       
              }
       		
}

paste("Accuracies for 10 fold cross validation result is:",accuracies)
paste("Average accuracy is:",mean(accuracies),"standard error is:",sd(accuracies))
 ## variable importance (optional)
 # load("model_custom.RData")
 # varImpPlot(modelFit,sort=T)
 

