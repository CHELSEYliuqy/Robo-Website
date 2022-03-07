MAXSER_HTV_factor_constraint <- function(X, F, factor.invest = TRUE, normalize_factors=TRUE, method="fraction", sigma, K=10, cv.huber=TRUE, grid.num=10, cv.huber.K=10, cv.gamma=TRUE, gamma.grid=20, cv.gamma.K=10, screening=FALSE, given.picks=NULL, Num.Screen=50,gamma.predict.method = "latest", given.gamma_all = NULL, given.gamma.predicted = NULL, given.Egamma2 = NULL, useKZ=TRUE, freq="daily", given.theta.all=NULL, risk.adj=FALSE, lev.adj=FALSE, core_num=NULL)
  ##Note: this version applies KZ to normailzed idiosyncratic returns to estimate theta_u
{
  # #library(limSolve)
  # library(lpSolve)
  # library(Rlab)
  # library(quadprog)
  # library(MASS)
  # library(PerformanceAnalytics)
  # library(FarmTest)
  # library(stats)
  
  # source("./source/SR.est.R")
  library(lars)
  
  ex <- 0
  
  # browser()
  ##############initial estimations##########################
  ##Estimate gamma's for self-normalizing
  ##need to perform Huber regression for estimating idiosyncratic returns first
  ##for self-normalization later, also need to estimate alpha based on stock mean and factor mean estimates via Huber mean
  
  T<-dim(X)[1]
  N<-dim(X)[2]
  f<-dim(F)[2]
  
  Beta<-matrix(0, nr=N, nc=f)
  Alpha<-matrix(0, nr=N, nc=1)
  
  thresh<-rep(0, N)
  probs.star<-rep(0,N)
  MSE.list<-vector("list", N)
  if(cv.huber==TRUE){
    num.solu<-grid.num
    probs.grid <- seq(0.9, 1, length.out = grid.num)
    for(i in 1:N){
      all.folds <- split(sample(1:T), rep(1:cv.huber.K, length = T))
      MSE<-matrix(0, nr=grid.num, nc=cv.huber.K)
      for(k in seq(cv.huber.K)){
        omit <- all.folds[[k]]
        F.train<-as.matrix(F[-omit, ])
        X.train<-X[-omit, i]
        F.test<-as.matrix(F[omit, ])
        X.test<-X[omit, i]
        infnorm.F<-apply(abs(F.train), 1, max)
        cv.grid<-quantile(infnorm.F, probs= probs.grid)
        for(j in 1:grid.num){
          if(j < grid.num){
            ex <- which(apply(abs(F.train), 1, max) > cv.grid[j])
            betas<-huber.reg(X=as.matrix(F.train[-ex, ]), Y=X.train[-ex], method = "adaptive")
          }
          if(j == grid.num) betas<-huber.reg(X=F.train, Y=X.train, method = "adaptive")
          MSE[j, k]<-mean((X.test-F.test%*%matrix(betas[-1])-betas[1])^2)
        }
      }
      MSE.list[[i]]<-MSE
      ind.star<-which.min(rowMeans(MSE))
      probs.star[i] <- probs.grid[ind.star]
      infnorm.F<-apply(abs(F), 1, max)
      cv.grid<-quantile(infnorm.F, probs= probs.grid)
      thresh[i]<-cv.grid[ind.star]
      
      if(probs.star[i]<1){
        ex <- which(apply(abs(F), 1, max) > thresh[i])
        huber.fit<-huber.reg(X=as.matrix(F[-ex, ]), Y=X[-ex, i], method = "adaptive")
      }
      if(probs.star[i]==1) huber.fit<-huber.reg(X=F, Y=X[,i], "adaptive")
      Beta[i, ]<-huber.fit[-1]
      Alpha[i]<-huber.fit[1]
    }
  }
  
  if(cv.huber==FALSE){
    for(i in 1:N){
      huber.fit<-huber.reg(X=F, Y=X[,i], method = "adaptive")
      Beta[i, ]<-huber.fit[-1]
      Alpha[i]<-huber.fit[1]
    }
  }
  
  U <- X - F %*% t(Beta)
  
  
  ##estimate gamma related based on full U and Alpha
  gamma2<-matrix(rowMeans((U-matrix(rep(1, T))%*%t(Alpha))^2))
  gamma<-sqrt(gamma2)
  if(is.null(given.gamma_all) == FALSE){
    gamma<-matrix(given.gamma_all)
    gamma2<-gamma^2
  }
  
  prob.gamma.star<-1
  if(cv.gamma==FALSE) E_gamma2<-mean(gamma2)
  if(cv.gamma==TRUE){
    prob.all<-seq(0.9, 1, length.out = gamma.grid)
    all.folds <- split(sample(1:T), rep(1:cv.gamma.K, length = T))
    MSE<-matrix(0, nr=gamma.grid, nc=cv.gamma.K)
    for(k in seq(cv.gamma.K)){
      omit <- all.folds[[k]]
      gamma2.train<-gamma2[-omit, ]
      gamma2.test<-gamma2[omit, ]
      M_T.all<-quantile(gamma2.train, probs= prob.all)
      for(j in 1:gamma.grid){
        if(j < gamma.grid){
          Egamma2.train<-mean(gamma2.train[-which(gamma2.train > M_T.all[j])])
        }
        if(j == grid.num) Egamma2.train<-mean(gamma2.train)
        MSE[j, k]<-mean((gamma2.test-Egamma2.train)^2)
      }
    }
    ind.star<-which.min(rowMeans(MSE))
    prob.gamma.star<-prob.all[ind.star]
    M_T.star<-quantile(gamma2, probs=prob.gamma.star)
    if(prob.gamma.star==1) E_gamma2<-mean(gamma2)
    if(prob.gamma.star<1) E_gamma2<-mean(gamma2[-which(gamma2 > M_T.star)])
  }
  if(is.null(given.Egamma2)==FALSE) E_gamma2 <- given.Egamma2
  
  picks<-0
  if(screening==TRUE){
    random.pick<-matrix(0, nr=100, nc=Num.Screen)
    random.theta<-rep(0, 100)
    for(g in 1:100) {
      random.pick[g,]<-sample(1:N, Num.Screen)
      random.data<-cbind(X[, random.pick[g,]],F)
      est<-theta.est(random.data,gamma2 = gamma2)
      random.theta[g]<-est$theta
    }
    
    loc<-order(random.theta)[95]
    picks<-random.pick[loc, ]
    
    if(is.null(given.picks)==FALSE) picks<-given.picks
    
    #IR<-(Alpha/matrix(apply(E, 2, sd)))^2
    #picks<-order(IR, decreasing = TRUE)[1:Num.Screen]
    X<-X[,picks]
    full.U<-U
    U<-U[,picks]
    full.Beta<-Beta
    Beta<-Beta[picks,]
    full.Alpha<-Alpha
    Alpha<-matrix(Alpha[picks])
    N<-Num.Screen
  }
  
  
  oneT <- matrix(1, nr=T, nc=1)
  
  ##Scenario I: without factor investing
  if(factor.invest==FALSE){
    
    ##data transformation
    X_all<-X
    mu.f<-matrix(colMeans(F))
    mu_all<-Alpha+Beta%*%mu.f
    X_all_c<-oneT%*%t(mu_all) + (X_all-oneT%*%t(mu_all))/matrix(gamma, nr=T, nc=dim(X_all)[2], byrow = FALSE)*sqrt(E_gamma2)
    
    ##estimate theta and response
    # theta.re<-theta.est(X,gamma2 = gamma2)
    # theta<-theta.re$theta
    if(useKZ==TRUE){
      theta.re<-SR.est(X_all_c)
      theta<-theta.re$theta.a[1]
    }
    r_hat<-sqrt(theta)
    r_c<-(1+theta)/sqrt(theta)*sigma
    Rc<-rep(r_c,T)
    
    
    ##weight computation
    all.folds <- split(sample(1:T), rep(1:K, length = T))
    lars.est<-lars(X_all_c,Rc,type="lar",normalize=FALSE,intercept=FALSE)
    num.solu<-dim(coef(lars.est))[1]
    residmat <- matrix(0, num.solu, K)
    zeta<-rep(0, K)
    for(i in seq(K)) {
      omit <- all.folds[[i]]
      fit <- lars(X_all_c[ - omit,,drop=FALSE  ], Rc[ - omit], type="lar",normalize=FALSE,intercept=FALSE)
      w<- coef(fit)  ###length(lambda)xp
      outfit <- X[omit, ]%*%t(w)
      if(length(omit)==1) outfit<-matrix(outfit,nrow=1)
      residmat[, i] <- abs(sigma-apply(outfit, 2, sd))    
      zeta[i]<-sum(abs(w[which.min(residmat[,i]),]))/sum(abs(w[dim(w)[1],]))
    }
    zeta.star<-mean(zeta)
    w.maxser<-matrix(predict.lars(lars.est, type = "coefficients", mode = "fraction", s=zeta.star)$coef)
    
    ###estimating quantities in TV weight formula
    theta_0<-theta*E_gamma2
    xi<-mean(rep(theta_0, T)/(gamma2+theta_0))
    if(gamma.predict.method == "latest") gamma.predicted <- gamma[T]
    if(gamma.predict.method == "AR") {
      ar.fit<-ar(gamma)
      pred.ar<-predict(ar.fit, n.ahead = 1)
      gamma.predicted<-pred.ar$pred[1]
    }
    if(is.null(given.gamma.predicted)==FALSE) gamma.predicted <- given.gamma.predicted
    tv_scalar <- sigma*sqrt(1/(xi-xi^2))/(gamma.predicted^2+theta_0)*E_gamma2
    
    #Perform MAXSER on U_c to estimate w_u.0
    r_c<-1+theta #for computing Sigma_u^-1alpha
    R_c<-rep(r_c, T)
    
    ##weight computation
    all.folds <- split(sample(1:T), rep(1:K, length = T))
    lars.est<-lars(X_all_c,R_c,type="lar",normalize=FALSE,intercept=FALSE)
    num.solu<-dim(coef(lars.est))[1]
    residmat.tv <- matrix(0, num.solu, K)
    zeta.tv<-rep(0, K)
    for(i in seq(K)) {
      omit <- all.folds[[i]]
      fit <- lars(X_all_c[ - omit,,drop=FALSE  ], R_c[ - omit], type="lar",normalize=FALSE,intercept=FALSE)
      w<- coef(fit)  ###length(lambda)xp
      outfit <- X[omit, ]%*%t(tv_scalar*w)
      if(length(omit)==1) outfit<-matrix(outfit,nrow=1)
      residmat.tv[, i] <- abs(sigma-apply(outfit, 2, sd))    
      zeta.tv[i]<-sum(abs(w[which.min(residmat.tv[,i]),]))/sum(abs(w[dim(w)[1],]))
    }
    zeta.tv.star<-mean(zeta.tv)
    w.mhtv<-tv_scalar*matrix(predict.lars(lars.est, type = "coefficients", mode = "fraction", s=zeta.tv.star)$coef)
    
    outlist<-list(probs.star, thresh, theta=theta, mu_all=mu_all, gamma=gamma, mu.f=mu.f, Alpha=Alpha, Beta=Beta, zeta.star=zeta.star,zeta.tv.star=zeta.tv.star, w.maxser=w.maxser,w.mhtv=w.mhtv, theta.re=theta.re)
  }
  
  ##Scenario II: with factor investing
  
  if(factor.invest==TRUE){
    
    #######################################################################
    #following are parallel weights based on factor Momentum
    ###estimate mean with momentum signal
    #if(freq=="weekly") ff<-F[(T-51):(T-4), ]
    #if(freq=="monthly") ff<-F[(T-11):(T-1), ]
    #if(freq=="daily") ff<-F[(T-251):(T-21), ]
    #mom<-matrix((apply(ff+1, 2, prod))^(1/dim(ff)[1])-1)
    #mu.f<-mom
    
    #if(normalize_factors==TRUE){
    # F_hat_c<-(F-oneT%*%t(mu.f))/matrix(gamma, nr=T, nc=f, byrow = FALSE)
    #  Sigma.f<-cov(F_hat_c)*E_gamma2
    #}
    #Sigma.f<-cov(F)
    
    #theta.f<-(t(mu.f)%*%solve(Sigma.f)%*%mu.f)[1]
    
    #theta.re<-theta.est(cbind(X,F), gamma2 = gamma2)
    #theta.all<-theta.re$theta
    
    #if(useKZ==TRUE){
    # X_all<-cbind(X, F)
    #   mu_all<-matrix(c(Alpha+Beta%*%mu.f, mu.f))
    #   X_all_c<-oneT%*%t(mu_all) + (X_all-oneT%*%t(mu_all))/matrix(gamma, nr=T, nc=dim(X_all)[2], byrow = FALSE)*sqrt(E_gamma2)
    #   theta.re<-SR.est(X_all_c)
    #   theta.all<-theta.re$theta.a[1]
    # }
    
    # if(is.null(given.theta.all)==FALSE) theta.all<-given.theta.all
    # 
    # theta.u<-theta.all-theta.f
    # if(theta.u<0) {
    #   theta.u<-0
    #   theta.all<-theta.f
    # }
    # theta_0.u<-theta.u*E_gamma2
    # 
    # U_c<-oneT%*%t(Alpha)+(U-oneT%*%t(Alpha))/matrix(gamma, nr=T, nc=N, byrow=FALSE)*sqrt(E_gamma2)
    # 
    # if(theta.u>0){
    #   #Perform MAXSER on U
    #   x.hat<-theta.u/theta.all
    #   r_c<-(1+theta.u)/sqrt(theta.u)
    #   R_c<-rep(r_c, T)
    #   lars.est<-lars(U_c,R_c,type="lar",normalize=FALSE,intercept=FALSE)
    #   all.folds <- split(sample(1:T), rep(1:K, length = T))
    #   num.solu<-dim(coef(lars.est))[1]
    #   residmat <- matrix(0, num.solu, K)
    #   zeta<-rep(0, K)
    #   for(i in seq(K)) {
    #     omit <- all.folds[[i]]
    #     fit <- lars(U_c[ - omit,,drop=FALSE  ], R_c[ - omit], type="lar",normalize=FALSE,intercept=FALSE)
    #     w.ucv<- (sqrt(x.hat)*sigma*coef(fit))   ###length(lambda)xp
    #     w.fcv<-matrix(0,nr=dim(w.ucv)[1], nc=f)
    #     for(s in 1:(dim(w.ucv)[1])) w.fcv[s,]<-c(sigma/sqrt(theta.all)*solve(Sigma.f)%*%mu.f-t(Beta)%*%matrix(w.ucv[s,]))
    #     
    #     outfit <- X[omit, ]%*%t(w.ucv)+as.matrix(F[omit, ])%*%t(w.fcv) ##HERE IS A Q ABOUT USING (X, F) OR (Y, F_hat)
    #     
    #     if(length(omit)==1) outfit<-matrix(outfit,nrow=1)
    #     residmat[, i] <- abs(sigma-apply(outfit, 2, sd))    
    #     frac<-rowSums(abs(coef(fit)))/sum(abs(coef(fit)[num.solu,]))
    #     #if(max(residmat[,i])<=0) chosen<-which.min(abs(residmat[,i]))
    #     #if(max(residmat[,i])>0) chosen<-which(residmat[,i] == min(residmat[residmat[,i]>=0,i]))
    #     zeta[i]<-frac[which.min(residmat[,i])]
    #   }
    #   zeta.star<-mean(zeta)
    #   w.A.m<-sqrt(x.hat)*sigma*predict(lars.est, type="coefficient", mode="fraction", s=zeta.star)$coef
    # }
    # 
    # if(theta.u==0){
    #   x.hat<-0
    #   r_c<-0
    #   zeta.star<-0
    #   w.A.m<-matrix(0, nr=N, nc=1)
    #   if(screening==TRUE) w.A.m<-matrix(0, nr=Num.Screen, nc=1)
    # }
    # 
    # w.F.m<-sigma/sqrt(theta.all)*solve(Sigma.f)%*%mu.f-t(Beta)%*%w.A.m
    # 
    # if(screening==TRUE){
    #   temp<-w.A.m
    #   Beta<-full.Beta
    #   U<-full.U
    #   Alpha<-full.Alpha
    #   N<-dim(U)[2]
    #   w.A.m<-matrix(0, nr=N, nc=1)
    #   w.A.m[picks,]<-temp
    # }
    # 
    # w.maxser.m<-matrix(c(w.A.m, w.F.m))
    # 
    # ###estimating quantities in TV weight formula
    # theta_0.all<-theta.all*E_gamma2
    # xi<-mean(rep(theta_0.all, T)/(gamma2+theta_0.all))
    # if(gamma.predict.method == "latest") gamma.predicted <- gamma[T]
    # if(gamma.predict.method == "AR") {
    #   ar.fit<-ar(gamma)
    #   pred.ar<-predict(ar.fit, n.ahead = 1)
    #   gamma.predicted<-pred.ar$pred[1]
    # }
    # if(is.null(given.gamma.predicted)==FALSE) gamma.predicted <- given.gamma.predicted
    # tv_scalar <- sigma*sqrt(1/(xi-xi^2))/(gamma.predicted^2+theta_0.all)*E_gamma2
    # 
    # w_f<-matrix(solve(Sigma.f)%*%mu.f)
    # 
    # if(theta.u>0){
    #   #Perform MAXSER on U_c to estimate w_u.0
    #   r_c<-1+theta.u #for computing Sigma_u^-1alpha
    #   R_c<-rep(r_c, T) 
    #   lars.est<-lars(U_c,R_c,type="lar",normalize=FALSE,intercept=FALSE)
    #   all.folds <- split(sample(1:T), rep(1:K, length = T))
    #   num.solu<-dim(coef(lars.est))[1]
    #   residmat.tv <- matrix(0, num.solu, K)
    #   zeta.tv<-rep(0, K)
    #   for(i in seq(K)) {
    #     omit <- all.folds[[i]]
    #     fit <- lars(U_c[ - omit,,drop=FALSE  ], R_c[ - omit], type="lar",normalize=FALSE,intercept=FALSE)
    #     w.ucv<- coef(fit)   ###length(lambda)xp
    #     w.fcv<-matrix(0,nr=dim(w.ucv)[1], nc=f)
    #     for(s in 1:(dim(w.ucv)[1])) w.fcv[s,]<-c(w_f-t(Beta)%*%matrix(w.ucv[s,]))
    #     
    #     outfit <- X[omit, ]%*%t(tv_scalar*w.ucv)+as.matrix(F[omit, ])%*%t(tv_scalar*w.fcv) #choose the one by the risk of the final weights
    #     
    #     if(length(omit)==1) outfit<-matrix(outfit,nrow=1)
    #     residmat.tv[, i] <- abs(sigma-apply(outfit, 2, sd))    
    #     frac<-rowSums(abs(coef(fit)))/sum(abs(coef(fit)[num.solu,]))
    #     #if(max(residmat[,i])<=0) chosen<-which.min(abs(residmat[,i]))
    #     #if(max(residmat[,i])>0) chosen<-which(residmat[,i] == min(residmat[residmat[,i]>=0,i]))
    #     zeta.tv[i]<-frac[which.min(residmat.tv[,i])]
    #   }
    #   zeta.tv.star<-mean(zeta.tv)
    #   w_u.tv.m<-predict(lars.est, type="coefficient", mode="fraction", s=zeta.tv.star)$coef
    # }
    # 
    # if(theta.u==0){
    #   r_c<-0
    #   zeta.tv.star<-0
    #   w_u.tv.m<-matrix(0, nr=N, nc=1)
    #   if(screening==TRUE) w_u.tv.m<-matrix(0, nr=Num.Screen, nc=1)
    # }
    # 
    # w.F.tv.m<-tv_scalar*(w_f-t(Beta)%*%w_u.tv.m)
    # w.A.tv.m<-tv_scalar*w_u.tv.m
    # 
    # if(screening==TRUE){
    #   temp<-w.A.tv
    #   Beta<-full.Beta
    #   U<-full.U
    #   Alpha<-full.Alpha
    #   N<-dim(U)[2]
    #   w.A.tv.m<-matrix(0, nr=N, nc=1)
    #   w.A.tv.m[picks,]<-temp
    # }
    # 
    # w.mhtv.m<-matrix(c(w.A.tv.m, w.F.tv.m))
    #######################################################################
    
    
    #factor mean & cov estimation and factor normalization
    mu.f<-matrix(colMeans(F))
    Sigma.f<-cov(F)
    
    if(normalize_factors==TRUE){
      F_hat_c <- (F-oneT%*%t(mu.f))/matrix(gamma, nr=T, nc=f, byrow = FALSE)
      Sigma.f <- cov(F_hat_c)*E_gamma2
    }
    
    theta.f<-(t(mu.f)%*%solve(Sigma.f)%*%mu.f)[1]
    w_fac_hat <- 1 / sqrt(theta.f) * solve(Sigma.f) %*% mu.f
    
    #theta.re<-theta.est(cbind(X,F), gamma2 = gamma2)
    #theta.all<-theta.re$theta
    
    if(is.null(given.theta.all)==FALSE) theta.all<-given.theta.all
    
    U_c <- oneT%*%t(Alpha)+(U-oneT%*%t(Alpha))/matrix(gamma, nr=T, nc=N, byrow=FALSE)*sqrt(E_gamma2)
    
    if(useKZ==TRUE){
      theta.re <- SR.est(U_c)
      theta.u <- theta.re$theta.a[1]
    }
    
    theta.all <- theta.u + theta.f
    if(theta.u<0) {
      theta.u<-0
      theta.all<-theta.f
    }
    theta_0.u<-theta.u*E_gamma2
    
    if(theta.u>0){
      #Perform MAXSER on U
      r_c <- (1+theta.u)/sqrt(theta.u)
      R_c <- rep(r_c, T)
      
      lars.est <- lasso.ineq(U_c, R_c, C.full=matrix(0, nrow=dim(U_c)[2]-1, ncol=dim(U_c)[2]), b=rep(0,(dim(U_c)[2]-1)), normalize=FALSE, intercept=FALSE, step=0.05)
      lars.est.cons <- colSums(lars.est$coefs >= 0)
      invalid.cons <- which(lars.est.cons < dim(U_c)[2])

      # last.nonzero <- sum(colSums(lars.est$coefs==0) != dim(U_c)[2])
      # if (length(invalid.cons == last.nonzero)){
      #   invalid.cons <- head(invalid.cons, -1)
      # }

      if (length(invalid.cons) > 0){
        lars.est$lambda[which(lars.est.cons < dim(U_c)[2])] <- Inf
      }
      
      num.solu <- dim(lars.est$coefs)[2]
      all.folds <- split(sample(1:T), rep(1:K, length = T))
      
      x.hat <- rep(w_fac_hat^2, num.solu) / (rep(w_fac_hat^2, num.solu)  + (t(lars.est$coefs) %*% Beta)^2)
      if (w_fac_hat >= 0){
        invalid.solu <- which(((t(lars.est$coefs)%*%Beta)^2 > rep(w_fac_hat^2,num.solu))==FALSE)
      }else{
        invalid.solu <- which(((t(lars.est$coefs)%*%Beta)^2 < rep(w_fac_hat^2,num.solu))==FALSE)
      }
      
      residmat <- matrix(0, num.solu, K)
      lambda <- rep(0, K)
      
      
      # browser()
      ###################################################################################################################################################
      # cl <- makeCluster(core_num)
      # registerDoParallel(cl)
      
      out <- foreach(i=seq(K), .combine='cbind', .packages='PACLasso') %do%{
        omit <- all.folds[[i]]
        fit <- lasso.ineq(U_c[ - omit,,drop=FALSE  ], R_c[ - omit], C.full=matrix(0, nrow=dim(U_c)[2]-1, ncol=dim(U_c)[2]), b=rep(0,(dim(U_c)[2]-1)), normalize=FALSE, intercept=FALSE, step=0.05)
        
        cons.result <- colSums(fit$coefs >= 0)
        invalid.cons <- which(lars.est.cons < dim(U_c)[2])
        
        w.ucv <- as.numeric(sqrt(x.hat) * sigma) * t(fit$coefs)
        w.fcv<-matrix(0, nr=dim(w.ucv)[1], nc=f)
        
        for (s in 1:dim(w.ucv)[1]){
          if (x.hat[s] != 1){
            w.fcv[s, ] <- sigma * sqrt(1-x.hat[s]) * w_fac_hat - as.numeric(t(Beta) %*% matrix(w.ucv[s,]))
          }else if (x.hat[s] == 1){
            w.fcv[s,] <- (-1) * t(Beta) %*% matrix(w.ucv[s,])
          }
        }
        
        w.fcv[which(w.fcv < 0)] = 0
        
        outfit <- X[omit, ] %*% t(w.ucv) + as.matrix(F[omit, ]) %*% t(w.fcv) ##HERE IS A Q ABOUT USING (X, F) OR (Y, F_hat)
        
        if(length(omit)==1) 
          outfit <- matrix(outfit, nrow=1)
        
        temp_risk <- apply(outfit, 2, sd)
        residmat[, i] <- abs(sigma - temp_risk)
        
        if (length(invalid.cons) > 0){
          residmat[, i][invalid.cons] <- Inf
        }
        
        lambda.temp <- fit$lambda[which.min(residmat[,i])]
        CV.risk.temp <- temp_risk[which.min(residmat[,i])]
        CV.lev.temp <- sigma / CV.risk.temp
        c(lambda.temp, CV.risk.temp, CV.lev.temp)
      }
      lambda <- out[1, ]
      CV.risk <- out[2, ]
      CV.lev <- out[3, ]
      
      # stopCluster(cl)
      ###################################################################################################################################################
      
      lambda.star <- mean(lambda)
      CV.risk.star <- mean(CV.risk)
      CV.lev.star <- mean(CV.lev)
      Risk.lev.star <- sigma / CV.risk.star
      lambda.diff <- abs(lars.est$lambda - lambda.star)
      lambda.idx <- which.min(lambda.diff)
      w_coef <- lars.est$coefs[, lambda.idx]
      x.hat.opt <- x.hat[lambda.idx]
      w.A  <- sqrt(x.hat.opt) * sigma * w_coef
    }
    
    
    if(theta.u<0 | theta.u==0){
      x.hat.opt <- 0
      r_c <- 0
      lambda.star <- 0
      w.A <- matrix(0, nr=N, nc=1)
      if(screening==TRUE) 
        w.A <- matrix(0, nr=Num.Screen, nc=1)
    }
    
    w.F <- sigma * sqrt(1-x.hat.opt) * w_fac_hat - as.numeric(t(Beta) %*% w.A)
    if (w.F + 1e-15 < 0){
      w.F = 0
    }
    
    
    if(screening==TRUE){
      temp<-w.A
      Beta<-full.Beta
      U<-full.U
      Alpha<-full.Alpha
      N<-dim(U)[2]
      w.A<-matrix(0, nr=N, nc=1)
      w.A[picks,]<-temp
    }
    
    if (risk.adj==TRUE & lev.adj==FALSE){
      leverage <- Risk.lev.star
    }else if (lev.adj==TRUE & risk.adj==FALSE){
      leverage <- CV.lev.star
    }else if (lev.adj==TRUE & risk.adj==TRUE){
      leverage <- (Risk.lev.star + CV.lev.star) / 2
    }else
      leverage <- 1
    
    w.A <- w.A * leverage
    w.F <- w.F * leverage
    
    w.maxser <- matrix(c(w.A, w.F))
    
    ###estimating quantities in TV weight formula
    ##this version used pure scaled MAXSER-H
    theta_0.all <- theta.all * E_gamma2
    xi <- mean(rep(theta_0.all, T) / (gamma2+theta_0.all))
    if(gamma.predict.method == "latest") gamma.predicted <- gamma[T]
    if(gamma.predict.method == "AR") {
      ar.fit<-ar(gamma)
      pred.ar<-predict(ar.fit, n.ahead = 1)
      gamma.predicted<-pred.ar$pred[1]
    }
    if(is.null(given.gamma.predicted)==FALSE) gamma.predicted <- given.gamma.predicted
    tv_scalar <- sqrt(theta.all/(xi-xi^2))*E_gamma2/(gamma.predicted^2+theta.all*E_gamma2)
    
    w.mhtv <- w.maxser*tv_scalar
    
    outlist<-list(leverage=leverage, prob.gamma.star=prob.gamma.star, probs.star=probs.star, thresh=thresh, MSE.list=MSE.list,picks=picks, w.maxser=w.maxser, w.F=w.F, w.A=w.A, w.mhtv=w.mhtv, tv_scalar=tv_scalar, x.hat=x.hat,theta.re=theta.re, theta.all=theta.all, theta.f=theta.f, theta.u=theta.u, Beta=Beta, Alpha=Alpha, Sigma.f=Sigma.f, lambda.star=lambda.star, mu.f=mu.f, gamma=gamma, E_gamma2=E_gamma2, normalize_factors=normalize_factors, gamma.predicted=gamma.predicted, gamma.predict.method=gamma.predict.method, useKZ=useKZ)
  }
  
  return(outlist)
}