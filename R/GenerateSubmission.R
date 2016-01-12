# Function to generate a submission file for competitions
GenerateSubmission <- function(predictions, filename, samplefilename){
    subm <- read.csv(samplefilename)
    subm$RESIGNED <- predictions
    write.csv(subm, file = paste("submissions/", filename, ".csv", sep = ""), row.names = F)
}