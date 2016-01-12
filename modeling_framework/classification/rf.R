
# codes/classification/rf.R
# not using caret interface

library(randomForest)
library(pROC)

source("codes/classification/preprocess.R")

# ====

# Random forest
rf0 <- randomForest(data = Xtrain, income ~., ntree = 300, importance = T, proximity = T)

# [rf0] Out-of-bag (OOB) error rate as a function of num. of trees:
plot(rf0$err.rate[,1], type = "l", lwd = 3, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")

# [rf0] tuning the mtry hyperparameter
tuneRF(subset(Xtrain, select = -income),
       Xtrain$income,
       ntreetry = 100)
title("Random forest: Tuning the mtry hyperparameter")

# [rf0] Variable importance
varImpPlot(rf0)

# [rf0] MDS
MDSplot(rf0, fac = Xtrain$income, palette = c("green", "red"), main = "Random forest: MDS")

# [rf0] Predictions using rf0
rf0_pred_prob <- predict(rf0, Xtest, type = "prob")
rf0_pred_class <- predict(rf0, Xtest, type = "response")

# [rf0] Confusion matrix
View(rf0_cm <- table(rf0_pred_class, Xtest$income))

# [rf0] Accuracy
print(rf0_acc <- sum(diag((rf0_cm))) / sum(rf0_cm))

# [rf0] ROC-AUC
print(rf0_auc <- auc(response = Xtest$income, predictor = rf0_pred_prob))
