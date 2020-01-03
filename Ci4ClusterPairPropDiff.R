

library(clust.bin.pair)

thyroids_data<- nested.to.contingency(thyroids$x.pet, thyroids$x.spect)


# method_one

Ci4ClusterPairPropDiff<-function(data, alpha, method){
  

    k<-dim(data)[1]
    
    # N
    n<-matrix(data$ak+data$bk+data$ck+data$dk, ncol=1)
    N<-apply(n, 2, sum)
    
    # p1
    ak_bk<-matrix(data$ak+data$bk, ncol=1)
    p1<-apply(ak_bk, 2, sum)/N
    
    # p2
    ak_ck<-matrix(data$ak+data$ck, ncol=1)
    p2<-apply(ak_ck, 2, sum)/N
    
    p=(p1+p2)/2
    
    # Use (p1+p2)/2 in calculating both variance and covariance
    if(method=="method1"){
        # varp1
        com1<-matrix((ak_bk-n*p)**2, ncol=1)
        varp1<-apply(com1, 2, sum)*k/((N**2)*(k-1))
        
        # varp2
        com2<-matrix((ak_ck-n*p)**2, ncol=1)
        varp2<-apply(com2, 2, sum)*k/((N**2)*(k-1))
        
        # covar
        com<-matrix((ak_bk-n*p)*(ak_ck-n*p), ncol=1)
        covar<-apply(com, 2, sum)*k/((N**2)*(k-1))
        
        # var
        var=varp1+varp2-2*covar
        se<-sqrt(var)
    
    }
    
    # Use p1 and p2 in calculating both variance and covariance
    if(method=="method2"){
      # varp1
      com1<-matrix((ak_bk-n*p1)**2, ncol=1)
      varp1<-apply(com1, 2, sum)*k/((N**2)*(k-1))
      
      # varp2
      com2<-matrix((ak_ck-n*p2)**2, ncol=1)
      varp2<-apply(com2, 2, sum)*k/((N**2)*(k-1))
      
      # covar
      com<-matrix((ak_bk-n*p1)*(ak_ck-n*p2), ncol=1)
      covar<-apply(com, 2, sum)*k/((N**2)*(k-1))
      
      # var
      var=varp1+varp2-2*covar
      se<-sqrt(var)
      
    }
    
    # Use (p1+p2)/2 in calculating individual variance and Use p1 and p2 for covariance
    if(method=="method3"){
      # varp1
      com1<-matrix((ak_bk-n*p)**2, ncol=1)
      varp1<-apply(com1, 2, sum)*k/((N**2)*(k-1))
      
      # varp2
      com2<-matrix((ak_ck-n*p)**2, ncol=1)
      varp2<-apply(com2, 2, sum)*k/((N**2)*(k-1))
      
      # covar
      com<-matrix((ak_bk-n*p1)*(ak_ck-n*p2), ncol=1)
      covar<-apply(com, 2, sum)*k/((N**2)*(k-1))
      
      # var
      var=varp1+varp2-2*covar
      se<-sqrt(var)
      
    }
    
    # effect size
    es<-p1-p2
    
    # upper_limit
    upper_limit<-es+qnorm(1-alpha/2)*se
    
    
    # lower_limit
    lower_limit<-es-qnorm(1-alpha/2)*se
    
    
    cat("Proportion difference =",  es, "\n", 
        "Variance =",  var, "\n",
        "Standard Error =",  se, "\n",
        100*(1-alpha),  "% Confidence Interval = [", lower_limit, ",", upper_limit, "]")
}

    
Ci4ClusterPairPropDiff(thyroids_data, 0.05, "method3")




