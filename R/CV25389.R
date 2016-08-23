
# modified function from stats.stackexchange
# http://stats.stackexchange.com/questions/25389/obtaining-predicted-values-y-1-or-0-from-a-logistic-regression-model-fit/25398#25398

CV25389 <- function(pred_prob, truth){
    
    perf <- function(cut, pred_prob, truth)
    {
        yhat = (pred_prob>cut)
        w = which(truth==1)
        sensitivity = mean( yhat[w] == 1 ) 
        specificity = mean( yhat[-w] == 0 ) 
        c.rate = mean( truth==yhat ) 
        d = cbind(sensitivity,specificity)-c(1,1)
        d = sqrt( d[1]^2 + d[2]^2 ) 
        out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
        colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
        return(out)
    }
    
    s = seq(.01,.99,length=1000)
    OUT = matrix(0,1000,4)
    for(i in 1:1000) OUT[i,]=perf(s[i],pred_prob,truth)
    plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
    axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
    axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
    lines(s,OUT[,2],col="darkgreen",lwd=2)
    lines(s,OUT[,3],col=4,lwd=2)
    lines(s,OUT[,4],col="darkred",lwd=2)
    box()
    legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))
    
}
