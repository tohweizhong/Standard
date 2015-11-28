# xgb.R

# Pipeline:
# @ Ensure preprocess.R and FE.R are sourced
# @ Change data structure to suitable for xgb.train() in xgboost pkg
# @ Construct model
# @ Inspection
#   - Variable importance
#   - Performance on training set
# @ Grid search tuning?

# ====

library(caret)
library(xgboost)
library(pROC)

source("codes/preprocess.R")
source("codes/FE.R")

## Want to use xgb.DMatrix
### First create matrices
train.mat <- as.matrix(subset(train, select = -income))
tune.mat  <- as.matrix(subset(tune, select = -income))
test.mat  <- as.matrix(subset(test, select = -income))

### Create the xgb.DMatrix objects
train.xgbd                  <- xgb.DMatrix(data = train.mat, label = train.labels, missing = NA)
tune.xgbd <- watchlist.xgbd <- xgb.DMatrix(data = tune.mat, label = tune.labels, missing = NA)
test.xgbd                   <- xgb.DMatrix(data = test.mat, label = test.labels, missing = NA)

### Watchlist
wl <- list(train = train.xgbd, wl = watchlist.xgbd)

## Finally, construct XGB model using xgb.train function
system.time(
    xgb0 <- xgb.train(data = train.xgbd, subsample = 0.6, colsample_bytree = 1/sqrt(ncol(train)-1),
                      max.depth = 3, eta = 0.15, nrounds = 1500, scale_pos_weight = 1/3,
                      nthread = 4, objective = "binary:logistic", verbose = 1,
                      eval.metric = "error", eval.metric = "logloss",
                      missing = NA, watchlist = wl)
)

## Variable importance
xgb.importance(feature_names = train.xgbd$data@Dimnames[[2]], model = xgb0)

## Predictions
xgb0.pred.prob <- predict(xgb0, test.xgbd)

## Performance
print(xgb0.auc <- auc(xgb0.pred.prob, test.labels))

## Tuning
### Grid
subsample        <- seq(0.5, 1, b = 0.1)
colsample_bytree <- seq(0.1, 1, 0.1)
max.depth        <- seq(1, 50, by = 1)
eta              <- seq(0.01, 0.3, by = 0.01)

tuneGrid <- expand.grid(
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    max.depth = max.depth,
    eta = eta
)

results <- NULL
for(i in seq(nrow(tuneGrid))){
    if(i %% 10 == 0) cat(paste("Iteration ", i, " ", sep = ""))
    params <- tuneGrid[i,]
    xgb0 <- xgb.train(data = train.xgbd, subsample = params$subsample, colsample_bytree = params$colsample_bytree,
                      max.depth = params$max.depth, eta = params$eta, nrounds = 800, scale_pos_weight = 1/3,
                      nthread = 4, objective = "binary:logistic", verbose = 0,
                      eval.metric = "error", eval.metric = "logloss",
                      missing = NA)
    
    # difference from above training code
    # @ verbose = 0
    # @ no watchlist
    
    xgb0.pred.prob <- data.frame(cbind(predict(xgb0, tune.xgbd), tune.labels))
    results <- c(results, auc(xgb0.pred.prob[,2], xgb0.pred.prob[,1]))
}
tuneGrid <- cbind(tuneGrid, results)
