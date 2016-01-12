# Function to push new element to end of list
PushList <- function(lst, newitem){
    
    lst[[length(lst) + 1]] <- newitem
    return(lst)
}