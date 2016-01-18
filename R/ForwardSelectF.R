
# Function to do stepwise regression
# using forward selection, based on F-tests

ForwardSelectF<-function(fullMod,alpha=0.05){
    require(stringr)
    df<-fullMod$model
    
    #find the response variable
    response<-str_sub(as.character(fullMod$call[2]),1,1)
    #empty model with beta0 only
    curMod<-lm(df[,response]~1,data=df)
    holdDf<-df[,response] #holdDf holds all variables in curMod
    
    done<-FALSE
    while(!done){
        addAnova<-add1(curMod,formula(df),test="F")
        pvalue<-min(addAnova$"Pr(>F)",na.rm=T)
        if(pvalue<alpha){
            #adding this predictor will significantly improve the model
            addMe<-rownames(addAnova)[which(addAnova$"Pr(>F)"==pvalue)]
            
            #update holdDf
            holdDf<-data.frame(holdDf,df[,addMe])
            colnames(holdDf)[1]<-response
            colnames(holdDf)[ncol(holdDf)]<-addMe
            
            #rebuild model
            curMod<-lm(formula(holdDf),data=df)
        }
        else done<-TRUE #no predictors can be dropped anymore
    }
    return(curMod)
}
