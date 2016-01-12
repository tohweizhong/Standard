# Function to convert a categorical variable to numerical,
# replacing the values with empirical probabilities
Cate2Prob <- function(one.col){
    tab <- table(one.col) / length(one.col)
    classes <- names(tab)
    
    # check
    if(!all(classes %in% unique(one.col))) stop("Something wrong with the classes")
    
    tmp.col <- NULL
    #     # use a simple for-loop for now
    #     # very slow...
    #     for(i in seq(length(one.col))){
    #         val <- one.col[i]
    #         idx <- which(classes ==  val)
    #         tmp.col <- c(tmp.col, tab[idx])
    #     }
    
    tmp.col <- sapply(one.col, FUN = function(x){
        idx <- which(classes == x)
        return(tab[idx])
    })
    
    
    # check
    if(length(tmp.col) != length(one.col)) stop("Lengths dont tally")
    return(tmp.col)
}