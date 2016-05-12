

# function to compute multi class log loss
# borrowed from MLmetrics::MultiLogLoss

MultiLogLoss2 <- function (y_pred, y_true){
    if (is.matrix(y_true) == FALSE) {
        y_true <- model.matrix(~0 + ., data.frame(as.character(y_true)))
    }
    eps <- 1e-15
    N <- nrow(y_pred)
    #y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
    
    for(i in seq(nrow(y_pred))){
        
        vec <- y_pred[i,]
        y_pred[i,which(vec < 0.01)] <- eps
        y_pred[i,which(vec > 0.99)] <- 1-eps
    }
    
    MultiLogLoss <- (-1/N) * sum(y_true * log(y_pred))
    return(MultiLogLoss)
}