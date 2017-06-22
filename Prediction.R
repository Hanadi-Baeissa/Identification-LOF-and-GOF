set.seed(12345)
library(plyr)
library(randomForest)
data_new <- read.csv("TestSet.csv",header=TRUE)

# following line merges column 1 and column 2 and create a new column named "rowN" which has Protein_ID and Mutation name together and seperated by '_'. you may choose any thing eg sep="_with_mutation_at" or sep ="," or sep =";" etc. If you will use sep="," then in final result.csv you have to shift column names and should write for your self.

data_new$rowN = paste(data_new$Protein.ID, data_new$Substitution, sep="_")
# store the created variable in new variable so we can use this to assign unique row names to our data
row_name <- data_new$rowN
# assign row names
row.names(data_new) <- row_name
# remove unnecessary columns
drops <- c("Protein.ID","Substitution","rowN")
data_new_1 <- data_new[ , !(names(data_new) %in% drops)] # remove second column (class) from data as we going to predict that

# load model
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
r1 <- rownames(data_new_1)
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
# assign field name for rows. It may be visualized when you save data in RData format.but write.csv ignores the rowname's heading/field name
names(dimnames(result)) <- c("ProteinID_Mutation", " ")
result <- as.matrix(result)
# right result in file
write.csv(result,"result.csv")
# in resultant csv file you have to manually write row name heading .eg ProteinId_Mutation or ProteinId_with_Mutation etc.


