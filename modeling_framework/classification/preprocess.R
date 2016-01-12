
# codes/classification/preprocess.R

library(caret)
library(DMwR)

Xtt <- read.csv("data/adult.csv", header = T, stringsAsFactors = T, strip.white = T)

# Rename values of the response variable to legal R variable names
# Doing predictions using caret pkg demands this
Xtt$income <- as.character(Xtt$income)
Xtt$income <- unlist(sapply(Xtt$income, FUN = function(x){
    if(x == ">50K") return("MoreThan50K")
    else if (x == "<=50K") return("NotMoreThan50K")
}))
Xtt$income <- factor(Xtt$income)

tr_idx <- createDataPartition(Xtt$income, p = 0.7, list = F)
Xtrain <- Xtt[tr_idx,]; Xtest <- Xtt[-tr_idx,]