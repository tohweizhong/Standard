# xgb.R

# Pipeline:
# @ Ensure preprocess.R and FE.R are sourced
# @ Construct model
# @ Inspection
#   - Variable importance
#   - Performance on training set
# @ Grid search tuning

library(xgboost)
library(caret)
library(Matrix)
library(pROC)

source("codes/preprocess.R")
source("codes/FE.R")

# ## Combine both training and tuning sets together
# ### This is because tuning is done within the training procedure in xgb (with cv)
# train <- rbind(train, tune)
# train.labels <- c(train.labels, tune.labels)

## Set up training parameters
trainCtrl <- trainControl(method = "cv",
                          number = 10,
                          repeats = 10)

## Set up parameter search space
### For xgb in caret, can only do expand.grid for the following:
### @ nrounds (# Boosting iterations)
### @ max_depth (Max Tree Depth)
### @ eta (Shrinkage)
### For all other parameters, tune using nested for-loops

tuneGrid.caret <- expand.grid(nrounds = c(15),
                              eta = c(0.1),
                              max_depth = c(1) # Tree stumps
                              )

## To set up a watchlist, use xgb.DMatrix
wl <- xgb.DMatrix(data = sparse.model.matrix(income ~., data = train), label = train.labels)
wl <- list(wl = wl)

## Tuning: use a for-loop over a manual grid
min_child_weight_grid <- seq(5, 50, by = 5)
subsample_grid        <- seq(0.5, 1, by = 0.1)
colsample_bytree_grid <- seq(0.1, 1, by = 0.1)

tuneGrid.manual <- expand.grid(min_child_weight = min_child_weight_grid,
                               subsample = subsample_grid,
                               colsample_bytree = colsample_bytree_grid)
nrow(tuneGrid.manual) # this determines how many iterations in the loop

### Need to record down the performances on the tuning set
### based on params referred by the for-loop

tune.performances <- NULL

for(i in seq(nrow(tuneGrid.manual))){
    if(i %% 1 == 0) cat(paste("Iteration ", i, " :: ", Sys.time(), "\n" ,sep = ""))
    xgb0 <- train(data = train, income ~ .,
                  method = "xgbTree",
                  trControl = trainCtrl,
                  tuneGrid = tuneGrid.caret
                  #watchlist = wl,
                  # add additional xgb params here:
                  ,objective = "binary:logistic"
                  #,silent = 1 # <--- duplicated parameter
                  ,nthread = 4
                  ## tuning the following:
                  ,min_child_weight = tuneGrid.manual$min_child_weight[i]
                  ,subsample        = tuneGrid.manual$subsample[i]
                  ,colsample_bytree = tuneGrid.manual$colsample_bytree[i]
    )
    
    # measure performance on the tuning set
    xgb0.pred.prob <- predict(xgb0, newdata = tune[, -ncol(tune)], type = "prob")[,1]
    tune.performances <- c(tune.performances,
                           auc(predictor = xgb0.pred.prob, response = tune.labels))
}

## Tuning performances
boxplot.stats(tune.performances)

## Set of parameters that optimise performance
best.param <- which(tune.performances == max(tune.performances))
tuneGrid.manual[best.param,]

tuneGrid.manual <- cbind(tuneGrid.manual, tune.performances)

## Now with the optimal set of parameters,
## tune towards point of overfit
## Specifically tuning nrounds and eta

tuneGrid.caret2 <- expand.grid(nrounds = seq(200, 800, by = 100),
                               max_depth = c(1),
                               eta = seq(0.01, 0.2, by = 0.03))
nrow(tuneGrid.caret2)

xgb1 <- train(data = train, income ~ .,
              method = "xgbTree",
              trControl = trainCtrl,
              tuneGrid = tuneGrid.caret2
              #watchlist = wl,
              # add additional xgb params here:
              ,objective = "binary:logistic"
              #,silent = 1 # <--- duplicated parameter
              ,nthread = 4
              ## tuning the following:
              ,min_child_weight = tuneGrid.manual$min_child_weight[best.param]
              ,subsample        = tuneGrid.manual$subsample[best.param]
              ,colsample_bytree = tuneGrid.manual$colsample_bytree[best.param]
)
plot(xgb1)

## Make predictions on testing set
xgb1.pred.prob <- predict(xgb1, newdata = test[, -ncol(test)], type = "prob")[,1]
auc(predictor = xgb1.pred.prob, response = test.labels)
