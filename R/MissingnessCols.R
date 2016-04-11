
# function to find the percentage of missingness in variables
# Return a vector of percentages for each variable
MissingnessCols <- function(df){
    return(apply(df, MARGIN = 2, function(x){
        
        numNAs <- x %>% is.na %>% length
        numRecords <- x %>% length
        return(numNAs / numRecords)
    }))
}