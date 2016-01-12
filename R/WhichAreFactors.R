# Function to identify which columns in a data.frame are factors
WhichAreFactors <- function(df){
    datatypes <- sapply(df, class)
    tmp <- NULL
    for(i in seq(length(datatypes))){
        if(datatypes[i] == "factor")
            tmp <- c(tmp, TRUE)
        else tmp <- c(tmp, FALSE)
    }
    return(tmp)
}