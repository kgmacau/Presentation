
#########################################################
####### Raw data                              ###########
#########################################################

library(boot)
library(caret)

value_tru<-c(rep(1, 5900), rep(0, 0), rep(1, 100), rep(0, 100))
value_pre<-c(rep(1, 5900), rep(1, 0), rep(0, 100), rep(0, 100))


rawdata<-data.frame(value_tru, value_pre)


### Factor
rawdata$value_tru<-factor(ifelse(rawdata$value_tru >0, "Positive", "Negative"))
rawdata$value_pre<-factor(ifelse(rawdata$value_pre >0, "Positive", "Negative"))
confusionMatrix(data = rawdata$value_pre, reference = rawdata$value_tru)
crosstable<-table(rawdata[, 2], rawdata[, 1])



#########################################################
####### Bootstraping CI for balanced accuracy ###########
#########################################################

bacc<- function(data, i) { 
  d=data[i,]
  bacc<-(sensitivity(d$value_pre, d$value_tru)+specificity(d$value_pre, d$value_tru))/2
  c(bacc)
}


bacc.boot<-boot(rawdata, bacc, R = 10000)
boot.ci(bacc.boot, conf=0.95, type = c("norm", "basic", "perc", "bca"))



################################################################
####### CI for balanced accuracy based on Chen Method###########
################################################################
baccCI<-function(data, alpha=0.05){
  
        bacc<-(sensitivity(data[, 2], data[, 1])+specificity(data[, 2], data[, 1]))/2
        
        crosstable<-table(data[, 2], data[, 1])
        a<-crosstable[1, 1]
        b<-crosstable[1, 2]
        c<-crosstable[2, 1]
        d<-crosstable[2, 2]
        p1<-(d/(b+d))**2
        p2<-a*c/((a+c)**3)
        p3<-(a/(a+c))**2
        p4<-b*d/((b+d)**3)
        varbacc<-p1*p2+p3*p4
        sebacc<-sqrt(varbacc)
        lowerCI<-bacc-qnorm(1-alpha/2)*sebacc
        upperCI<-bacc+qnorm(1-alpha/2)*sebacc
        
        x<-a+d
        n<-a+b+c+d
        
        # https://github.com/cran/PropCIs/blob/master/R/exactci.R
        # Clopper pearson exact method
        
        if (lowerCI<=0){
          lowerCI <- 1/(1 + (n - x + 1) / (x * qf(alpha/2, 2 * x, 2 * (n-x+1))))
        }
        
        if (upperCI>=1){
          upperCI<-1/(1 + (n - x) / ((x + 1) * qf(1-alpha/2, 2 * (x+1), 2 *(n-x))))
        }
        
        cat(" Balanced accuracy =",  bacc, "\n", 
            "standard error =",  sebacc, "\n",
            "95% frequentist CI for balanced accuracy = [", lowerCI, ",", upperCI, "]")
        
}       

baccCI(rawdata)        



################################################################
####### meta analysis of balanced accuracy using micp###########
################################################################
library(micp)
ks<-rbind(rep(80, 2), rep(80, 2))
ns<-rbind(rep(120, 2), rep(160, 2))
micp.stats(ks, ns)



################################################################
####### meta analysis of balanced accuracy using beta prior#####
################################################################
library(micp)
ks<-rbind(c(1, 80), c(1, 80))
ns<-rbind(c(2, 120), c(2, 160))
micp.stats(ks, ns)


##### CI for sensitivity and specificity
library(bdpv)
Tab8<-matrix(c(80, 40, 80, 80), ncol=2)
BDtest(xmat=Tab8, pr=0.5, conf.level = 0.95)
a<-Tab8[1, 1]
b<-Tab8[1, 2]
c<-Tab8[2, 1]
d<-Tab8[2, 2]
senlower<-(a/(a+c))-qnorm(1-0.05/2)*sqrt(a*c/((a+c)**3))
senupper<-(a/(a+c))+qnorm(1-0.05/2)*sqrt(a*c/((a+c)**3))

## https://stat.ethz.ch/pipermail/r-help/2012-February/303977.html
## https://pages.uoregon.edu/flournoy/bootstrapping/bootstrapexample.html
