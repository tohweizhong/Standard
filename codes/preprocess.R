# preprocess.R

# Objectives:
# @ Import data
# @ Drop columns
# @ Imputation
# @ Datatype conversions (e.g. categorical <-> numerical, relevel factors)
# @ Preparation for CV

# ====

library(caret)

source("codes/houseblend.R")

df <- read.csv("data/adult.csv", header = T, stringsAsFactors = T, strip.white = T)
### Response variable should be in the last column

## Drop unnecessary columns
df <- subset(df, select = -fnlwgt)

## Try to rename values of the response variable to legal R variable names
## Doing predictions using caret pkg demands this
df$income <- as.character(df$income)
df$income <- sapply(df$income, FUN = function(x){
    if(x == ">50K") return("MoreThan50K")
    else if (x == "<=50K") return("NotMoreThan50K")
})
df$income <- factor(df$income)


## Converting nominal variables to numerical
## (260815: neglecting ordinal variables for now)
## Few methods available:
## @ One-hot encoding
## @ Empirical probabilities (of each class)
## @ Chi-squared contributions

### Use Empirical probabilities here
factorIdx <- WhichAreFactors(df)
#### Dont convert the response variable
factorIdx[length(factorIdx)] <- FALSE
system.time(df[,factorIdx] <- apply(df[,factorIdx], MARGIN = 2, FUN = Cate2Prob))



## Split the dataset into 3 partitions
### 60% training, 20% tuning, 20% testing
set.seed(86647)
trainIdx <- createDataPartition(df$income, p = 0.6, list = F)
train    <- df[trainIdx,]

tmp     <- df[-trainIdx,]
tuneIdx <- createDataPartition(tmp$income, p = 0.5, list = F)
tune    <- tmp[tuneIdx,]
test    <- tmp[-tuneIdx,]

nrow(train) + nrow(tune) + nrow(test) == nrow(df)

### Labels
train.labels <- as.numeric(train$income) - 1
tune.labels  <- as.numeric(tune$income) - 1
test.labels  <- as.numeric(test$income) - 1

# ====

# Return values of this script
# @ train
# @ tune
# @ test
# @ train.labels
# @ tune.labels
# @ test.labels

