
# Function to compute CEL
# require a data.frame with the following columns
# @ first column: predicted probability to resign
# @ second column: correct answers
ComputeCEL <- function(df){
    One.row.CEL <- function(one.row){
        y_i <- one.row[1]
        p_i <- one.row[2]
        p_i <- max(min(p_i, 1 - 10^-15), 10^-15)
        return(y_i * log10(p_i) + (1 - y_i)*log10(1 - p_i))
    }
    total.CEL <- 0
    for(i in seq(nrow(df))){
        one.row <- df[i,]
        total.CEL <- total.CEL + One.row.CEL(one.row)
    }
    return(-total.CEL/nrow(df))
}

# example
#ComputeCEL(data.frame(rbind(c(1,0.5),c(0, 0.5), c(1, 0.5), c(0, 0.5))))
# this should be 0.30103