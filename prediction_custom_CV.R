set.seed(12345)
library(plyr)
library(randomForest)
data_new <- read.csv("AllData.csv",header=TRUE,row.names = 1)
drops <- c("X.1") # as this column has empty values
data_new_1 <- data_new[ , !(names(data_new) %in% drops)] # remove second column (class) from data as we going to predict that
load("model_custom.RData")


# handle missing values to median. Can also use mean , mode 
impute.value <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
data.new <- sapply(data_new_1, function(x){
    if(is.numeric(x)){
            impute.value(x)
        } else {
            x
        }
    }
)

r1 <- rownames(data_new)
rownames(data.new) <- r1



# now prediction 

predicted <- predict(modelFit,data.new)

# change leveles 1 and 0 as OG and TS  
levels(predicted) <- c("OG","TS")

# for confidence about prediction extract probabilities for being in that class
predicted_prob <- predict(modelFit,data.new,type="prob")

colnames(predicted_prob) <- c("OG","TS")
# combined both objects for clear picture
result <- cbind(as.character(predicted),predicted_prob)
colnames(result) <- c("prediction","OG","TS")
result <- as.data.frame(result)
# right result in file
write.csv(result,"result.csv")

