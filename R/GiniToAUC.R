
# GiniToAUC.R
# Function to compute AUC from Gini

GiniToAUC <- function(g){
    auc <- (g + 1)/2
    return(auc)
}
