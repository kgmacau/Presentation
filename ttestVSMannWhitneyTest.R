



power_comparison<- function (simulation, samplesize, mean1, mean2, sigma1, sigma2, distribution, shape, rate, scale, shape1, shape2){
        
      set.seed(123456789)
      x<-do.call(rbind, lapply(1:simulation, function(i) {
        
        
        z_test = function(a, b, sigma.a, sigma.b){
          n.a = length(a)
          n.b = length(b)
          z = (mean(a) - mean(b)) / (sqrt(sigma.a**2/n.a + sigma.b**2/n.b))
          p.value=2*pnorm(-abs(z))
          
          return(p.value)
        }
        
        
        if (distribution=="normal"){

          dt1<-rnorm(samplesize, mean = mean1, sd = sigma1)
          dt2<-rnorm(samplesize, mean = mean2, sd = sigma2)
          
          c(pvalue_ztest=z_test(dt1, dt2, sigma1, sigma2),
            pvalue_ttest=t.test(dt1, dt2)$p.value,
            pvalue_mwtest=wilcox.test(dt1, dt2)$p.value)
          }
        
        else if (distribution=="gamma"){
          dt1<-rgamma(samplesize, shape, rate)
          dt2<-rgamma(samplesize, shape, rate)
          
          c(pvalue_ztest=z_test(dt1, dt2, sd(dt1), sd(dt2)),
            pvalue_ttest=t.test(dt1, dt2)$p.value,
            pvalue_mwtest=wilcox.test(dt1, dt2)$p.value)
        }
        
        else if (distribution=="weibull"){
          dt1<-rweibull(samplesize, shape1, scale)
          dt2<-rweibull(samplesize, shape2, scale)
          
          c(pvalue_ztest=z_test(dt1, dt2, sd(dt1), sd(dt2)),
            pvalue_ttest=t.test(dt1, dt2)$p.value,
            pvalue_mwtest=wilcox.test(dt1, dt2)$p.value)
        }
        
        else if (distribution=="beta"){
          dt1<-rbeta(samplesize, shape1, shape2)
          dt2<-rbeta(samplesize, shape1, shape2)
          
          c(pvalue_ztest=z_test(dt1, dt2, sd(dt1), sd(dt2)),
            pvalue_ttest=t.test(dt1, dt2)$p.value,
            pvalue_mwtest=wilcox.test(dt1, dt2)$p.value)
        }
        
        else if (distribution=="log_normal"){

          dt1<-rlnorm(samplesize, meanlog = log(mean1), sdlog = log(sigma1))
          dt2<-rlnorm(samplesize, meanlog = log(mean2), sdlog = log(sigma2))
        
          c(pvalue_ztest=z_test(dt1, dt2, sd(dt1), sd(dt2)),
            pvalue_ttest=t.test(dt1, dt2)$p.value,
            pvalue_mwtest=wilcox.test(dt1, dt2)$p.value, 
            pvalue_lztest=z_test(log(dt1), log(dt2), log(sigma1), log(sigma2)),
            pvalue_lttest=t.test(log(dt1), log(dt2))$p.value)
          }
        
      }))
      
      ##########################
      ### power of z-test #
      ##########################
      ztest<-ifelse(x[, 1]<=0.05, 1, 2)
      power_ztest<-table(ztest==1)[names(table(ztest==1))=="TRUE"]/simulation
      
      ##########################
      ### power of t-test #
      ##########################
      ttest<-ifelse(x[, 2]<=0.05, 1, 2)
      power_ttest<-table(ttest==1)[names(table(ttest==1))=="TRUE"]/simulation
      
      ##########################
      ### power of mann-whitney-test #
      ##########################
      mwtest<-ifelse(x[, 3]<=0.05, 1, 2)
      power_mwtest<-table(mwtest==1)[names(table(mwtest==1))=="TRUE"]/simulation

      if (distribution=="log_normal"){
        ##########################
        ### power of z-test with log-scale trans#
        ##########################
        lztest<-ifelse(x[, 4]<=0.05, 1, 2)
        power_lztest<-table(lztest==1)[names(table(lztest==1))=="TRUE"]/simulation
        
        ##########################
        ### power of t-test with log-scale trans#
        ##########################
        lttest<-ifelse(x[, 5]<=0.05, 1, 2)
        power_lttest<-table(lttest==1)[names(table(lttest==1))=="TRUE"]/simulation
        
        c(power_ztest=power_ztest, 
          power_ttest=power_ttest, 
          power_mwtest=power_mwtest,
          power_lztest=power_lztest, 
          power_lttest=power_lttest)
      }
      
    else {
      c(# power_ztest=power_ztest
        power_ttest=power_ttest, 
        power_mwtest=power_mwtest)}
}


##################################################################################
################ Draw two samples from normal distribution #######################
##################################################################################


# Example 1: small mean, large and equal variance, large sample size
power_comparison(10000, 1000,  0.1, 0.2, 1, 1, "normal")

# Example 2: small mean, large and equal variance, moderate sample size
power_comparison(10000, 100, 0.1, 0.2, 1, 1, "normal")

# Example 3: small mean, large and equal variance, small sample size
power_comparison(10000, 20,  0.1, 0.2, 1, 1, "normal")



power_comparison(10000, 1000,  0.1, 0.1, 1, 1, "normal")
power_comparison(10000, 100, 0.1, 0.1, 1, 1, "normal")
power_comparison(10000, 20,  0.1, 0.1, 1, 1, "normal")



# Example 4: small mean, large and unequal variance, large sample size
power_comparison(10000, 1000, 0.1, 0.2, 1, 2, "normal")

# Example 5: small mean, large and unequal variance, moderate sample size
power_comparison(10000, 100, 0.1, 0.2, 1, 2, "normal")

# Example 6: small mean, large and unequal variance, small sample size
power_comparison(10000, 20, 0.1, 0.2, 1, 2, "normal")


power_comparison(10000, 1000,  0.1, 0.1, 1, 2, "normal")
power_comparison(10000, 100, 0.1, 0.1, 1, 2, "normal")
power_comparison(10000, 20,  0.1, 0.1, 1, 2, "normal")



# Example 7: large mean, small and unequal variance, large sample size
power_comparison(10000, 1000, 11, 12, 1, 2, "normal")

# Example 8: large mean, small and unequal variance, moderate sample size
power_comparison(10000, 100, 11, 12, 1, 2, "normal")

# Example 9: large mean, small and unequal variance, small sample size
power_comparison(10000, 20, 11, 12, 1, 2, "normal")

power_comparison(10000, 1000,  11, 11, 1, 2, "normal")
power_comparison(10000, 100, 11, 11, 1, 2, "normal")
power_comparison(10000, 20,  11, 11, 1, 2, "normal")

##################################################################################
################ Draw two samples from non-normal distribution ###################
##################################################################################

# Example 10: weibull distribution
dt_weib<-rweibull(100, 1, 1000)
hist(dt_weib)


power_comparison(simulation=10000, samplesize=1000, distribution="weibull", shape1=1, shape2=2, scale=1000)
power_comparison(simulation=10000, samplesize=100, distribution="weibull", shape1=1, shape2=2, scale=1000)
power_comparison(simulation=10000, samplesize=20, distribution="weibull", shape1=1, shape2=2, scale=1000)

power_comparison(simulation=10000, samplesize=1000, distribution="weibull", shape1=1, shape2=1, scale=1000)
power_comparison(simulation=10000, samplesize=100, distribution="weibull", shape1=1, shape2=1, scale=1000)
power_comparison(simulation=10000, samplesize=20, distribution="weibull", shape1=1, shape2=1, scale=1000)


# Example 11: Log-normal distribution
dt_log<-rlnorm(100, log(2), log(3))
hist(dt_log)

power_comparison(10000, 1000, 2, 3, 3, 3, "log_normal")
power_comparison(10000, 100, 2, 3, 3, 3, "log_normal")
power_comparison(10000, 20, 2, 3, 3, 3, "log_normal")

power_comparison(10000, 1000, 2, 2, 3, 3, "log_normal")
power_comparison(10000, 100, 2, 2, 3, 3, "log_normal")
power_comparison(10000, 20, 2, 2, 3, 3, "log_normal")

# beta distribution
# power_comparison(simulation=10000, samplesize=1000, distribution="beta", shape1=2, shape2=2000)

x<-do.call(rbind, lapply(1:10000, function(i) {
  
  
  dt<-rweibull(100, 1, 1000)
  
  mean=mean(dt)
  
  c(mean=mean)
  
}))
  
hist(x)

help(rlnorm)
### http://www.ucd.ie/ecomodel/Resources/Sheet4_data_distributions_WebVersion.html
### https://www.r-graph-gallery.com/
### https://thestatsgeek.com/2014/04/12/is-the-wilcoxon-mann-whitney-test-a-good-non-parametric-alternative-to-the-t-test/
