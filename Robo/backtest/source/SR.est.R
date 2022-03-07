SR.est<-function(X)
{
  
  Bxab<-function(x,a,b){
    value<-pbeta(q=x, shape1=a, shape2=b)*beta(a,b)
    return(value)
  }
  
    
    p <- ncol(X)
    n <- nrow(X)
    y <- p/n
    r <- 1/(1-y)
    k<-y/(1-y)
    
    x_avg <- matrix(colMeans(X))
    S<-cov(X)*(n-1)/n
    omega <- solve(S)
    one <- rep(1,p)
    theta.sample <- (t(x_avg)%*%omega%*%x_avg)
    theta<-(theta.sample[1]-k)/r
    
    theta.u<-((n-p-2)*theta.sample-p)/n
    
    theta.a<-theta.u+(2*theta.sample^(p/2)*(1+theta.sample)^(1-n/2))/(n*Bxab(x=theta.sample/(1+theta.sample), a=p/2, b=(n-p)/2))
    
    SR.a<-sqrt(theta.a)
    
    outlist<-list(theta.sample=theta.sample, theta.u=theta.u, theta.a=theta.a, SR.a=SR.a)
  return(outlist)
}