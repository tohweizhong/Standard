
# Function to get the unique values of each column in a data.frame
# returns a list

GetUnique <- function(df){
    
    require(magrittr)
    returnMe <- sapply(df, FUN = function(x){
        x %>% unique
    })
    return(returnMe)
    
}