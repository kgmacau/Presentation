

install.packages("xlsx")
library("xlsx")
library(knitr)
library(BSDA)
library(mvtnorm)


####### Type I Error ################

####################################################################################################
##### step 1: Draw two correlated normal distribution samples using rmvnorm functionvia simulations
##### step 2: Calculate the whole type I error of three methods
####################################################################################################
multiplicity_type1error<- function (simulation, samplesize, cov, mean1, mean2, sigma1, sigma2){
  
  x<-do.call(rbind, lapply(1:simulation, function(i) {
    sigma <- matrix(cov, nrow = 2)
    x <- rmvnorm(samplesize, mean = c(mean1, mean2), sigma = sigma, method = "chol")
    
    x1<-x[, 1]; 
    x2<-x[, 2];
    
    c(p1=z.test(x=x1, y=NULL, mu=mean1, sigma.x=sigma1, sigma.y=NULL, alternative = "two.sided")$p.value, 
      p2=z.test(x=x2, y=NULL, mu=mean2, sigma.x=sigma2, sigma.y=NULL, alternative = "two.sided")$p.value)
  }))
  
  
  ##########################
  ### Bonferroni Adjustment#
  ##########################
  Bon<-ifelse(x[, 1]<=0.05/2 | x[, 2]<=0.05/2, 1, 2)
  Bon_T1Err<-table(Bon==1)[names(table(Bon==1))=="TRUE"]/simulation
  
  
  ###############################################
  ### sequence adjustment ############
  ###############################################
  seq<-ifelse(x[, 1]<=0.05, 1, 2)
  seq_T1Err<-table(seq==1)[names(table(seq==1))=="TRUE"]/simulation
  
  
  #########################
  ### Hochberg Adjustment #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  Hochberg<-ifelse(xmax<=0.05 | (xmax>0.05 & xmin<=0.025), 1, 2)
  Hochberg_T1Err<-table(Hochberg==1)[names(table(Hochberg==1))=="TRUE"]/simulation
  
  
  #########################
  ### Holm Adjustment #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  holm<-ifelse(xmin<=0.025, 1, 2)
  holm_T1Err<-table(holm==1)[names(table(holm==1))=="TRUE"]/simulation
  
  
  
  c(Bon_T1Err=Bon_T1Err, 
    seq_T1Err=seq_T1Err,
    Hochberg_T1Err=Hochberg_T1Err, 
    holm_T1Err=holm_T1Err)
  
}


corr<-matrix(seq(0, 1, by=0.05))
error<-matrix(rep(0, 4*length(corr)), nrow=length(corr))
error<-do.call(rbind, lapply(1:length(corr), function(i){

  set.seed(1234567)
  multiplicity_type1error(100000, 200, c(4, 4*corr[i], 4*corr[i], 4), 0, 0, 2, 2)
  
}))


id<-seq(1, length(corr), by=1)
error_table<-data.frame(id, corr, error)
colnames(error_table)<-c("id", "Correlation", "Bonferroni", "Fixed-sequence", 
                                               "Hochberg", "Holm")
kable(error_table)



plot(error_table$Correlation, error_table$Bonferroni, type="b", ylim=range(0.02, 0.06), ylab="Type I Error", xlab="Correlation", col="green", main="Type I Error of Multiplicity Adjustment Methods")
lines(error_table$Correlation, error_table$Hochberg, type="b", col="red")
lines(error_table$Correlation, error_table$Holm, type="b", col="green")
legend("bottomleft", c("Hochberg","Bonferroni, Holm"), fill=c("red","green"))





########## Power ##########

####################################################################################################
##### step 1: Draw two correlated normal distribution samples using rmvnorm functionvia simulations
##### step 2: Calculate the whole power of three methods
####################################################################################################
multiplicity_power<- function (simulation, samplesize, cov, mean1, mean2, u1, u2, sigma1, sigma2){
  
  x<-do.call(rbind, lapply(1:simulation, function(i) {
    sigma <- matrix(cov, nrow = 2)
    x <- rmvnorm(samplesize, mean = c(mean1, mean2), sigma = sigma, method = "chol")
    
    x1<-x[, 1]; 
    x2<-x[, 2];
    
    c(p1=z.test(x=x1, y=NULL, mu=u1, sigma.x=sigma1, sigma.y=NULL, alternative = "two.sided")$p.value, 
      p2=z.test(x=x2, y=NULL, mu=u2, sigma.x=sigma2, sigma.y=NULL, alternative = "two.sided")$p.value)
  }))
  
  
  ##########################
  ### Bonferroni Adjustment#
  ##########################
  Bon<-ifelse(x[, 1]<=0.05/2 | x[, 2]<=0.05/2, 1, 2)
  Bon_power<-table(Bon==1)[names(table(Bon==1))=="TRUE"]/simulation
  
  
  
  #########################
  ### Hochberg Adjustment #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  Hochberg<-ifelse(xmax<=0.05 | (xmax>0.05 & xmin<=0.025), 1, 2)
  Hochberg_power<-table(Hochberg==1)[names(table(Hochberg==1))=="TRUE"]/simulation
  
  
  #########################
  ### Holm Adjustment #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  holm<-ifelse(xmin<=0.025, 1, 2)
  holm_power<-table(holm==1)[names(table(holm==1))=="TRUE"]/simulation
  
  
  
  c(Bon_power=Bon_power, 
    Hochberg_power=Hochberg_power, 
    holm_power=holm_power)
  
}


corr<-matrix(seq(0, 1, by=0.05))
power<-matrix(rep(0, 3*length(corr)), nrow=length(corr))
power<-do.call(rbind, lapply(1:length(corr), function(i){
  
  set.seed(1234567)
  multiplicity_power(100000, 200, c(4, 4*corr[i], 4*corr[i], 4), 0.4, 0.4, 0, 0, 2, 2)
  
}))


id<-seq(1, length(corr), by=1)
power_table<-data.frame(id, corr, power)
colnames(power_table)<-c("id", "Correlation", "Bonferroni", "Hochberg", "Holm")
kable(power_table)


plot(power_table$Correlation, power_table$Bonferroni, type="b", ylab="Power", xlab="Correlation", col="green", main="Power of Multiplicity Adjustment Methods")
lines(power_table$Correlation, power_table$Hochberg, type="b", col="red")
lines(power_table$Correlation, power_table$Holm, type="b", col="green")
legend("bottomleft", c("Hochberg","Bonferroni, Holm"), fill=c("red","green"))





########## Two win Power ##########

####################################################################################################
##### step 1: Draw two correlated normal distribution samples using rmvnorm functionvia simulations
##### step 2: Calculate the power of three methods
####################################################################################################
multiplicity_tpower<- function (simulation, samplesize, cov, mean1, mean2, u1, u2, sigma1, sigma2){
  
  x<-do.call(rbind, lapply(1:simulation, function(i) {
    sigma <- matrix(cov, nrow = 2)
    x <- rmvnorm(samplesize, mean = c(mean1, mean2), sigma = sigma, method = "chol")
    
    x1<-x[, 1]; 
    x2<-x[, 2];
    
    c(p1=z.test(x=x1, y=NULL, mu=u1, sigma.x=sigma1, sigma.y=NULL, alternative = "two.sided")$p.value, 
      p2=z.test(x=x2, y=NULL, mu=u2, sigma.x=sigma2, sigma.y=NULL, alternative = "two.sided")$p.value)
  }))
  
  
  ##########################
  ### Bonferroni Adjustment#
  ##########################
  Bon<-ifelse(x[, 1]<=0.05/2 & x[, 2]<=0.05/2, 1, 2)
  Bon_power<-table(Bon==1)[names(table(Bon==1))=="TRUE"]/simulation
  
  
  
  #########################
  ### Hochberg Adjustment #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  Hochberg<-ifelse(xmax<=0.05, 1, 2)
  Hochberg_power<-table(Hochberg==1)[names(table(Hochberg==1))=="TRUE"]/simulation
  
  
  #########################
  ### Holm Adjustment #
  #########################
  xmax<-apply(x, 1, max)
  xmin<-apply(x, 1, min)
  holm<-ifelse(xmin<=0.025 & xmax<=0.05, 1, 2)
  holm_power<-table(holm==1)[names(table(holm==1))=="TRUE"]/simulation
  
  
  
  c(Bon_power=Bon_power, 
    Hochberg_power=Hochberg_power, 
    holm_power=holm_power)
  
}


corr<-matrix(seq(0, 1, by=0.05))
tpower<-matrix(rep(0, 3*length(corr)), nrow=length(corr))
tpower<-do.call(rbind, lapply(1:length(corr), function(i){
  
  set.seed(1234567)
  multiplicity_tpower(100000, 200, c(4, 4*corr[i], 4*corr[i], 4), 0.4, 0.4, 0, 0, 2, 2)
  
}))


id<-seq(1, length(corr), by=1)
tpower_table<-data.frame(id, corr, tpower)
colnames(tpower_table)<-c("id", "Correlation", "Bonferroni", "Hochberg", "Holm")
kable(tpower_table)


plot(tpower_table$Correlation, tpower_table$Bonferroni, type="b", ylim=range(0.5, 0.85), ylab="Power", xlab="Correlation", col="green", main="Power of Multiplicity Adjustment Methods")
lines(tpower_table$Correlation, tpower_table$Hochberg, type="b", col="red")
lines(tpower_table$Correlation, tpower_table$Holm, type="b", col="Blue")
legend("bottomright", c("Hochberg","Bonferroni", "Holm"), fill=c("red","green", "blue"))

# https://aosmith.rbind.io/2018/08/29/getting-started-simulating-data/
# https://bookdown.org/rdpeng/rprogdatascience/simulation.html
# https://github.com/cran/MultiRNG/tree/master/R