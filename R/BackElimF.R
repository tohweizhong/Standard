
# Function to do stepwise regression
# using backward elimination, based on F-tests

BackElimF<-function(fullMod,alpha=0.05){
    df<-fullMod$model
    curMod<-fullMod
    
    done<-FALSE
    while(!done){
        dropAnova<-drop1(curMod,formula(df),test="F")
        pvalue<-max(dropAnova$"Pr(>F)",na.rm=T)
        if(pvalue>alpha){
            #dropping this predictor leads to no significant difference
            #between initial and reduced model
            dropMe<-rownames(dropAnova)[which(dropAnova$"Pr(>F)"==pvalue)]
            df<-df[,-which(colnames(df)==dropMe)]
            curMod<-lm(formula(df),data=df)
        }
        else done<-TRUE #no predictors can be dropped anymore
    }
    return(curMod)
}
