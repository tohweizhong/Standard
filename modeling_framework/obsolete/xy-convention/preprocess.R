# preprocess.R

# Objectives in preprocess step:
# @ Import data
# @ Drop columns
# @ Imputation
# @ Datatype conversions (e.g. categorical <-> numerical, relevel factors)
# @ Preparation for CV

library(caret)
library(DMwR)

source("C:/Users/weizhong/Documents/R/STANDARD-WORKFLOW/codes/xy-convention/houseblend.R")

Xtrain <- read.csv("data/titanic/train.csv")
Xtest <- read.csv("data/titanic/test.csv")

Ytrain <- Xtrain$Survived
Xtrain <- subset(Xtrain, select = -Survived)

Xtt <- rbind(Xtrain, Xtest) # combined
Xtrain.idx <- 1:nrow(Xtrain)
Xtest.idx <- nrow(Xtrain) + 1:nrow(Xtest)

## Drop redundant variables
Xtt <- subset(Xtt, select = -c(PassengerId, Name))

## Convert the values of "Survived" to valid R variable names
## This is a requirement of caret

Ytrain <- sapply(Ytrain, FUN = function(x){
    if(x == 0) return("No")
    else return("Yes")
})

## Convert "Survived" to factor
Ytrain <- as.factor(Ytrain)


## For Ticket, engineer a variable for duplicate tickets vs. not
EngDuplicateTix <- function(df){
    
    tab <- table(df$Ticket)
    duplicates <- names(which(tab > 1))
    
    tmp <- NULL
    for(i in seq(nrow(df))){
        if(df$Ticket[i] %in% duplicates){
            tmp <- c(tmp, 1)
        }
        else tmp <- c(tmp, 0)
    }
    df$DuplicateTix <- tmp
    
    return(df)
}

Xtt <- EngDuplicateTix(Xtt)

### Drop "Ticket"
Xtt <- subset(Xtt, select = -Ticket)

## Convert categorical variables to empirical probabilities
factor.idx <- WhichAreFactors(Xtt)
system.time(Xtt[,factor.idx] <- apply(Xtt[,factor.idx], MARGIN = 2, FUN = Cate2Prob))


## Imputation using knn
### Use default settings
Xtt <- knnImputation(Xtt)

## Done with all processing, split Xtt into Xtrain and Xtest
Xtrain <- Xtt[Xtrain.idx,]
Xtest <- Xtt[Xtest.idx,]

## Split Xtrain into 3 partitions
### 60% training, 20% tuning, 20% testing
set.seed(86647)
train.idx <- createDataPartition(Ytrain, p = 0.6, list = F) # Use Ytrain here
xtrain    <- Xtrain[train.idx,]
ytrain    <- Ytrain[train.idx]

xmp      <- Xtrain[-train.idx,]
ymp      <- Ytrain[-train.idx]

tune.idx <- createDataPartition(ymp, p = 0.5, list = F)
xtune    <- xmp[tune.idx,]
xtest    <- xmp[-tune.idx,]

ytune    <- ymp[tune.idx]
ytest    <- ymp[-tune.idx]

nrow(xtrain) + nrow(xtune) + nrow(xtest) == nrow(Xtrain)

rm(xmp, ymp)