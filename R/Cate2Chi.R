# Function to convert a categorical variable to numerical,
# replacing the values with chi-squared contributions

# Inputs:
# @ one.col: vector of categorical variable, may be factor or character
# @ y: binary response variable may be factor or character

Cate2Chi <- function(one.col, y){
    
    # check
    if(length(unique(y)) != 2)
        stop("Cate2Chi only works for binary response variables")
    
    # check
    if(class(one.col) != "factor" && class(one.col) != "character")
        stop("Cate2Chi only works for categorical variables")
    
    tab <- table(one.col, y)
    chi_sq_test <- chisq.test(tab)
    chi_sq <- chi_sq_test$statistic
    
    observed <- tab
    expected <- chi_sq_test$expected
    
    chi_sq_contri <- ((observed - expected) ** 2) / expected
    chi_sq_contri <- chi_sq_contri / chi_sq
    
    sum_across <- rowSums(chi_sq_contri)
    
    tmp.col <- sum_across[one.col]
    
    # check
    if(length(tmp.col) != length(one.col)) stop("Lengths dont tally")
    
    return(tmp.col)
}

# example usage
# df <- read.csv("data/adult.csv", stringsAsFactors = F)
# str(df)
# one.col <- df$education # 16 unique classes
# y <- df$income
# foo <- Cate2Chi(one.col = one.col, y = y)