# Function to generate one-hot encoded variables for all variables in a data.frame
OneHotEncode <- function(df, type = "train",  yvar){
    
    if(type == "train"){
        y  <- df[, which(colnames(df) == yvar)]
        df <- df[,-which(colnames(df) == yvar)]
        
        mm <- model.matrix(~ 0 + ., data = df)
        df <- cbind(y, data.frame(mm))
        colnames(df)[1] <- yvar
        return(df)
    }
    else if(type == "test"){
        return(data.frame(model.matrix(~ 0 + ., data = df)))
    }
    
}
