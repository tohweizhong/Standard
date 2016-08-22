
# AUCToGini.R
# Function to compute Gini from AUC

AUCToGini <- function(a){
    g <- 2*auc - 1
    return(g)
}
