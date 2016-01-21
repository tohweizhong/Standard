
# Function to find number of unique values of each column in a data.frame
# returns a vector

NumUnique <- function(df){
 
    require(magrittr)
    returnMe <- apply(df, MARGIN = 2, FUN = function(x){
        return(x %>% unique %>% length)
    })
    return(returnMe)
}