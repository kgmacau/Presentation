

library(TippingPoint)

tippingdat<-read.csv( "C:/Users/212697818/Desktop/tpdata.csv")


TippingPoint(outcome=tippingdat$binary, treat= tippingdat$treat,
             group.infor=TRUE, plot.type = "p.value",ind.values = TRUE,
             summary.type = "density", alpha = 0.95, S=1.5, 
             HistMeanT = c(0.38,0.4), HistMeanC =  c(0.2,0.55))


dt<- subset(tippingdat, binary==1 | binary==0)


# prop.test(table(dt$treat, dt$binary), correct=FALSE) 

prop.test(x = c(36, 20), n = c(40, 40), correct=TRUE)

prop.test(x = c(45, 25), n = c(50, 50), correct=TRUE)

prop.test(x = c(36, 30), n = c(50, 50), correct=TRUE)



# http://www.metafor-project.org/doku.php/tips:multiple_imputation_with_mice_and_metafor
# https://www.statmethods.net/management/subset.html