# Function to pop last item from list
PopList <- function(lst){
    
    lst_length <- length(lst)
    if(lst_length > 1)
        return(lst[1:lst_length - 1])
    else if(lst_length == 1)
        return(list())
}