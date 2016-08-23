
# Function to do stepwise regression based on F-tests

StepRegF<-function(fullMod,alphaEnter=0.05,alphaStay=0.05, response = ""){
    require(stringr)
    df<-fullMod$model
    
    # #find the response variable
    # response<-str_sub(as.character(fullMod$call[2]),1,1)
    # # only works when the length of the name of the response variable is one!
    
    #empty model with beta0 only
    curMod<-lm(df[,response]~1,data=df)
    holdDf<-df[,response] #holdDf holds all variables in curMod
    
    done<-FALSE
    while(!done){
        addAnova<-add1(curMod,formula(df),test="F")
        pvalue<-min(addAnova$"Pr(>F)",na.rm=T)
        if(pvalue<alphaEnter){
            #adding this predictor will significantly improve the model
            addMe<-rownames(addAnova)[which(addAnova$"Pr(>F)"==pvalue)]
            
            #update holdDf
            holdDf<-data.frame(holdDf,df[,addMe])
            colnames(holdDf)[1]<-response
            colnames(holdDf)[ncol(holdDf)]<-addMe
            
            #rebuild model
            curMod<-lm(formula(holdDf),data=df)
            
            #check if any predictor needs to leave
            dropAnova<-drop1(curMod,formula(holdDf),test="F")
            pvalue<-max(dropAnova$"Pr(>F)",na.rm=T)
            if(pvalue>alphaStay){
                #dropping this predictor leads to no significant difference
                #between initial and reduced model
                dropMe<-rownames(dropAnova)[which(dropAnova$"Pr(>F)"==pvalue)]
                holdDf<-holdDf[,-which(colnames(holdDf)==dropMe)]
                curMod<-lm(formula(holdDf),data=df)
            }
        }
        else done<-TRUE
    }
    return(curMod)
}
