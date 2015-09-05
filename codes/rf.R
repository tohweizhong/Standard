# rf.R

library(randomForest)
library(pROC)

source("codes/preprocess.R")
source("codes/FE.R")

## Construct model
rf0 <- randomForest(data = train, income ~., importance = T)

## Variable importance
varImpPlot(rf0)

## Tuning mtry and ntrees
plot(rf0$err.rate[,1], type = "l", lwd = 3, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")

### Do this for both training and tuning set
tuneRF(subset(train, select = -income),
       train$income,
       ntreetry = 100)
title("Random forest: Tuning the mtry hyperparameter")

tuneRF(subset(tune, select = -income),
       tune$income,
       ntreetry = 100)
title("Random forest: Tuning the mtry hyperparameter")

## Reconstruct model
rf1 <- randomForest(data = train, income ~., importance = T,
                    mtry = 2, ntrees = 251)

## Predictions
### Both probabilities and actual classes
rf1.pred.prob <- predict(rf1, subset(test, select = -income), type = "prob")[,2]
rf1.pred.class <- predict(rf1, subset(test, select = -income), type = "response")

## Performance
### confusion matrix
print(rf1.pred.conf <- table(rf1.pred.class, test$income))

### Accuracy
print(rf1.acc <- sum(diag((rf1.pred.conf))) / sum(rf1.pred.conf))

### ROC-AUC
print(rf1.auc <- auc(rf1.pred.prob, test.labels))

## Repeat procedure for untuned model and see what happens
rf0.pred.prob <- predict(rf0, subset(test, select = -income), type = "prob")[,2]
rf0.pred.class <- predict(rf0, subset(test, select = -income), type = "response")
print(rf0.pred.conf <- table(rf0.pred.class, test$income))
print(rf0.acc <- sum(diag((rf0.pred.conf))) / sum(rf0.pred.conf))
print(rf0.auc <- auc(rf0.pred.prob, test.labels))

## Other things can be explored for tuning:
## @ Max tree depth
## @ 