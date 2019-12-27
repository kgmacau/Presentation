

library(clust.bin.pair)

thyroids_data<- nested.to.contingency(thyroids$x.pet, thyroids$x.spect)


CI_clustered_paired_prop_diff<-function(data, alpha){
  
  data$n<-data$ak+data$bk+data$ck+data$dk
  
  # bk - ck
  bk_ck<-data.frame(data$bk-data$ck)
  colnames(bk_ck)<-"bk_ck"
  bk_ck_sum<-sapply(es, sum)
  
  # N
  n<-data.frame(data$n)
  colnames(n)<-"n"
  N<-sapply(n, sum)
  
  # effect_size
  effect_size<-bk_ck_sum/N
  
  
  k<-dim(data)[1]
  
  
  # var_com
  var_com<-data.frame((data$bk-data$ck-bk_ck_sum/k)**2)
  colnames(var_com)<-"var_com"
  var<-sapply(var_com, sum)*k/((k-1)*N**2)
  se<-sqrt(var)
  
  # upper_limit
  upper_limit<-effect_size+qnorm(1-alpha/2)*se
  
  
  # lower_limit
  lower_limit<-effect_size-qnorm(1-alpha/2)*se
  
  
  cat("Proportion difference =",  effect_size, "\n", 
      "standard error =",  se, "\n",
      100*(1-alpha),  "% confidence interval = [", lower_limit, ",", upper_limit, "]")
}

CI_clustered_paired_prop_diff(thyroids_data, 0.05)


# Reference
# 1. Obuchowski NA. On the comparison of correlated proportions for
#                clustered data. 

# 2. Yang Z, Sun XZ, Hardin JW. Confidence intervals for the difference of marginal 
#               probabilities in clusetered matched-pair binary data. 

# 3. https://works.bepress.com/zyang/
