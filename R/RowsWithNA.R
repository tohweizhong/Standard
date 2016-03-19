# Function to check which sample has NA
# Return a vector of logicals indicating whether a sample has NA or not
RowsWithNA <- function(df){
    return(apply(df, MARGIN = 1, function(x) any(is.na(x))))
}