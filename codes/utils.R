
# utils.R


# function to push new element to end of list
list.push <- function(lst, newitem){
    
    lst[[length(lst) + 1]] <- newitem
    return(lst)
}

# function to pop last item from list
list.pop <- function(lst){
    
    lst_length <- length(lst)
    if(lst_length > 1)
        return(lst[1:lst_length - 1])
    else if(lst_length == 1)
        return(list())
}
