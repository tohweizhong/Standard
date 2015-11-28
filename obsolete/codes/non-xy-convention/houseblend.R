# houseBlend.R

# Houses various functions that are commonly used
# @ ComputeCEL
# @ Cate2Prob
# @ WhichAreFactors
# @ GenerateSubmission

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
ComputeCEL(data.frame(rbind(c(1,0.5),c(0, 0.5), c(1, 0.5), c(0, 0.5))))
# this should be 0.30103




# Function to generate a submission file for competitions
GenerateSubmission <- function(predictions, filename){
    subm <- read.csv('docs/HR_Retention_2013_SampleSubmission.csv')
    subm$RESIGNED <- predictions
    write.csv(subm, file = paste("submissions/", filename, ".csv", sep = ""), row.names = F)
}



# Function to convert a categorical variable to numerical,
# replacing the values with empirical probabilities
Cate2Prob <- function(one.col){
    tab <- table(one.col) / length(one.col)
    classes <- names(tab)
    
    # check
    if(!all(classes %in% unique(one.col))) stop("Something wrong with the classes")
    
    tmp.col <- NULL
    # use a simple for-loop for now
    for(i in seq(length(one.col))){
        val <- one.col[i]
        idx <- which(classes ==  val)
        tmp.col <- c(tmp.col, tab[idx])
    }
    
    # check
    if(length(tmp.col) != length(one.col)) stop("Lengths dont tally")
    return(tmp.col)
}

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
