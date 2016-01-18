
# Function to do model selection (linear models)
# using exhaustive search
# metrics can be R2 or Cp

LMSelect<-function(fullMod,option=c("R2","Cp")){
    require(leaps)
    require(stringr)
    df<-fullMod$model
    #find the response variable
    response<-str_sub(as.character(fullMod$call[2]),1,1)
    X<-model.matrix(fullMod)[,-1]
    
    if(option=="R2") allR2<-leaps(X,df[,response],method="r2")
    else if(option=="Cp") allCp<-leaps(X,df[,response],method="Cp")
    
    #output models
    for(i in 2:ncol(dat)){
        if(option=="R2"){
            maxR2<-max(allR2$r2[allR2$size==i])
            whichModel<-allR2$which[allR2$r2==maxR2,]
            nameModel<-names(whichModel)[whichModel==T]
            cat(nameModel,"\n",maxR2,"\n")
        }
        else if(option=="Cp"){
            best<-min(abs(allCp$Cp[allCp$size==i]-i))
            bestCp<-abs(best-i)
            whichModel<-allCp$which[abs(allCp$Cp-i)==best,]
            nameModel<-names(whichModel)[whichModel==T]
            cat(nameModel,"\n",bestCp,"\n")
        }
    }
}
