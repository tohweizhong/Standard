# Function to check which variable has NA
# Return a vector of logicals indicating whether a variable has NA or not
ColsWithNA <- function(df){
    return(apply(df, MARGIN = 2, function(x) any(is.na(x))))
}