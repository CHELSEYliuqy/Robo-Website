MAXSER_weight_CV_Constraint<-function(X, F=NULL, screening=FALSE, Num.Screen=50, sigma, K=10, given.picks=NULL, given.theta = NULL, thres.value=-1, risk.adj=FALSE)
  ###This is the function for computing MAXSER portfolio weight based on the approach proposed in RFS paper "Approaching mean-variance efficiency for large portfolios" by Mengmeng Ao, Yingying Li and Xinghua Zheng. Coded by Mengmeng Ao.
  ###Arguments:
  ###X is T*N stock return matrix, F is the T*f matrix of factor returns, sigma is risk constraint, and K is number of folds for cross validatation
  ###Setting "screening=TRUE" will trigger the Step 0 screening process (described in the paper), and Num.Screen is the number of stocks after screening; if a certain set of picked stocks is going to be used, set "given.picks" to be a numeric vector containing the indexes of the picked stocks (in terms of column numbers)
  ###Special notice: if F is not specified (default setting), MAXSER portfolio weight without factor investing ("Scenario I" in the paper) will be computed. As long as F is specified, factor investing will be included in MAXSER strategy.
{
  
  library(lars)  ##For direct use of the codes here, package "lars" must be installed in R for computing lasso solutions; alternative packages such as "glmnet" can also be used with proper modification of the lasso function
  
  ##define the function for computing theta and maximum Sharpe ratio estimates
  SR.est<-function(X)  ##X is a matrix of returns based on which theta estimate is computed
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
    
    outlist<-list(theta.sample=theta.sample, theta.a=theta.a, SR.a=SR.a)
    ###among the outputs, theta.a is the final (adjusted) estimate of theta and SR.a is the corresponding estimate of the maximum Sharpe ratio
    return(outlist)
  }
  
  ##Scenario I: without factor investing
  if(is.null(F)==TRUE){
    T<-dim(X)[1]
    N<-dim(X)[2]
    
    picks<-0
    if(screening==TRUE){
      random.pick<-matrix(0, nr=1000, nc=Num.Screen)
      random.theta<-rep(0, 1000)
      for(g in 1:1000) {
        random.pick[g,]<-sample(1:N, Num.Screen)
        random.data<-X[, random.pick[g,]]
        random.theta[g]<-SR.est(random.data)$theta.a
      }
      
      loc<-order(random.theta)[950]
      picks<-random.pick[loc, ]
      
      if(is.null(given.picks)==FALSE) picks<-given.picks
      
      X<-X[,picks]
    }
    
    if (is.null(given.theta)) theta<-(SR.est(X)$theta.a)[1]
    else theta <- given.theta
    
    
    r_hat<-sqrt(theta)
    r_c<-(1+theta)/sqrt(theta)*sigma
    Y<-rep(r_c,T)
    
    
    all.folds <- split(sample(1:T), rep(1:K, length = T))
    
    
    lars.est<-lars(X,Y,type="lar",normalize=FALSE,intercept=FALSE)
    num.solu<-dim(coef(lars.est))[1]
    residmat <- matrix(0, num.solu, K)
    zeta<-rep(0, K)
    
    for(i in seq(K)) {
      omit <- all.folds[[i]]
      fit <- lars(X[ - omit,,drop=FALSE  ], Y[ - omit], type="lar",normalize=FALSE,intercept=FALSE)
      w<- coef(fit)  ###length(lambda)xp
      outfit <- X[omit, ]%*%t(w)
      if(length(omit)==1) outfit<-matrix(outfit,nrow=1)
      # residmat[, i] <- abs(sigma-apply(outfit, 2, sd))
      # zeta[i]<-sum(abs(w[which.min(residmat[,i]),]))/sum(abs(w[dim(w)[1],]))
      zeta[i]<-sum(abs(w[which.min( abs(sigma-apply(outfit, 2, sd)) ),]))/sum(abs(w[dim(w)[1],]))
    }
    zeta.star<-mean(zeta)
    w.maxser<-matrix(predict.lars(lars.est, type = "coefficients", mode = "fraction", s=zeta.star)$coef)
    
    if(screening==TRUE){
      temp<-w.maxser
      w.maxser<-matrix(0, nr=p, nc=1)
      w.maxser[picks,]<-temp
    }
    
    outlist<-list(picks=picks, w.maxser=w.maxser, theta=theta, zeta.star=zeta.star)
  }
  
  
  ##Scenario II: with factor investing
  if(is.null(F)==FALSE){
    T<-dim(X)[1]
    N<-dim(X)[2]
    f<-dim(F)[2]
    Beta<-matrix(0, nr=N, nc=f)
    Alpha<-matrix(0, nr=N, nc=1)
    picks<-0
    
    for(i in 1:N) {
      fit<-lm(X[,i]~F)
      Beta[i,]<-fit$coef[-1]
      Alpha[i]<-fit$coef[1]
    }
    E<-X-F%*%t(Beta)
    Sigma.e<-cov(E)
    # Omega.e<-solve(Sigma.e)
    
    if(screening==TRUE){
      random.pick<-matrix(0, nr=1000, nc=Num.Screen)
      random.theta<-rep(0, 1000)
      for(g in 1:1000) {
        random.pick[g,]<-sample(1:N, Num.Screen)
        random.data<-X[, random.pick[g,]]
        est<-SR.est(cbind(random.data, F))
        random.theta[g]<-est$theta.a
      }
      
      loc<-order(random.theta)[950]
      picks<-random.pick[loc, ]
      
      if(is.null(given.picks)==FALSE) picks<-given.picks
      
      X<-X[,picks]
      full.E<-E
      E<-E[,picks]
      full.Beta<-Beta
      Beta<-Beta[picks,]
    }
    
    mu.f<-colMeans(F)
    Sigma.f<-cov(F)
    theta.f<-(t(mu.f)%*%solve(Sigma.f)%*%matrix(mu.f))[1]
    
    if (is.null(given.theta)) {
      # theta<-(SR.est(cbind(X,F))$theta.a)[1]
      theta <- (SR.est(cbin(X,F))$theta.sample)[1]
      theta.u <- theta-theta.f

      #theta.u <- (SR.est(E)$theta.a)[1]
      #theta.u <- (SR.est(E)$theta.sample)[1]
      #theta <- theta.u + theta.f
    } else {
      theta.u <- given.theta
      theta <- theta.u + theta.f
    }
    
    if(theta.u>0){
      
      x.hat<-theta.u/theta
      
      r_hat<-sqrt(theta.u)
      r_c<-(1+theta.u)/sqrt(theta.u)
      Y<-rep(r_c,T)
      lars.est<-lars(E,Y,type="lar",normalize=FALSE,intercept=FALSE)
      # print(summary(lars.est))
      all.folds <- split(sample(1:T), rep(1:K, length = T))
      num.solu <- dim(coef(lars.est))[1]
      residmat <- matrix(0, num.solu, K)
      zeta <- rep(0, K)
      CV.risk <- rep(0, K)
      w.fcv.record <- matrix(0,nr=num.solu, nc=K)
      for(i in seq(K)) {
        omit <- all.folds[[i]]
        fit <- lars(E[ - omit,,drop=FALSE  ], Y[ - omit], type="lar",normalize=FALSE,intercept=FALSE)
        w.ucv <- (sqrt(x.hat)*sigma*coef(fit))
        w.fcv <- matrix(0,nr=dim(w.ucv)[1], nc=f)
        if(x.hat!=1) 
        {for(s in 1:(dim(w.ucv)[1])) w.fcv[s,]<- sigma/sqrt(theta)*solve(Sigma.f)%*%mu.f-t(Beta)%*%matrix(w.ucv[s,]) }
        if(x.hat==1)
        {for(s in 1:(dim(w.ucv)[1])) w.fcv[s,]<- (-1)*t(Beta)%*%matrix(w.ucv[s,]) }
        w.fcv.record[, i] <- w.fcv
        
        ########remove extreme weigths########
        #rm.idx <- which(w.fcv < thres)
        #w.fcv <- as.matrix(w.fcv[-rm.idx])
        #w.ucv <- w.ucv[-rm.idx, ]
        #if (is.null(dim(w.ucv)[1]))   w.ucv <- t(as.matrix(w.ucv))
        #
        #w.ucv <- w.ucv[which(w.fcv > thres),]
        #if (is.null(dim(w.ucv)[1]))   w.ucv <- t(as.matrix(w.ucv))

        w.ucv <- w.ucv[which(w.fcv > thres.value), ]
        if (is.null(dim(w.ucv)[1]))   w.ucv <- t(as.matrix(w.ucv))

        w.fcv <- as.matrix(w.fcv[which(w.fcv > thres.value)])
        ######################################
        
        
        outfit <- X[omit, ]%*%t(w.ucv)+F[omit, ]%*%t(w.fcv)
        if(length(omit)==1) outfit<-matrix(outfit,nrow=1)

        temp_risk <- apply(outfit, 2, sd)
        temp_diff <- abs(sigma - temp_risk)
        if (length(temp_diff) < num.solu){
          temp_diff <- c(temp_diff, rep(9900, (num.solu-length(temp_diff))))
        }
        
        residmat[, i] <- temp_diff
        frac <- rowSums(abs(coef(fit)))/sum(abs(coef(fit)[num.solu,]))
        zeta[i] <- frac[which.min(residmat[,i])]
        CV.risk[i] <- temp_risk[which.min(residmat[,i])]
        # frac<-rowSums(abs(coef(fit)))/sum(abs(coef(fit)[dim(coef(fit))[1],]))
        # zeta[i]<-frac[which.min( abs(sigma-apply(outfit, 2, sd)) )]
      }
      zeta.star <- mean(zeta)
      CV.risk.star <- mean(CV.risk)
      leverage <- sigma / CV.risk.star
      w.A <- sqrt(x.hat) * sigma * predict(lars.est, type="coefficient", mode="fraction", s=zeta.star)$coef
      w_coef <- predict(lars.est, type="coefficient", mode="fraction", s=zeta.star)$coef
      l1norm <- sum(abs(w_coef))
      rmse <- sqrt(mean((Y - E %*% w_coef)^2))
    }
    
    if(theta.u<0 | theta.u==0){
      x.hat<-0
      theta.u<-0
      w.A<-matrix(0, nr=N, nc=1)
      if(screening==TRUE) w.A<-matrix(0, nr=Num.Screen, nc=1)
      rmse <- 0
      lars.est <- NULL
      r_c <- NA
      zeta.star <- 0
      l1norm <- 0
    }
    
    w.F <- sigma/sqrt(theta.f+theta.u)*solve(Sigma.f)%*%mu.f-t(Beta)%*%w.A
    if(x.hat==1) w.F<-(-1)*t(Beta)%*%w.A
    
    if(screening==TRUE){
      temp<-w.A
      w.A<-matrix(0, nr=N, nc=1)
      w.A[picks,]<-temp
      Beta<-full.Beta
      E<-full.E
    }

    if (risk.adj){
      w.A <- w.A * leverage
      w.F <- w.F * leverage
    }

    w.maxser<-matrix(c(w.A, w.F))
    
    outlist <- list(lars.est=lars.est, zeta.star = zeta.star, l1norm = l1norm,
                    picks=picks, w.maxser=w.maxser, w.F=w.F, w.A=w.A,
                    x.hat=x.hat, theta=theta, theta.f=theta.f, theta.u=theta.u, Beta=Beta, Alpha=Alpha, E=E,
                    rmse=rmse, r_c=r_c, w.fcv.record=w.fcv.record, leverage=leverage)
  }
  
  return(outlist)
}



