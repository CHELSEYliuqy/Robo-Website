#' Maximum-Sharpe-ratio Estimation and sparse Regression with Cross-Validation Constraint
#'
#' This is the function for computing MAXSER portfolio weight based on the approach proposed in RFS paper "Approaching mean-variance efficiency for large portfolios" by Mengmeng Ao, Yingying Li and Xinghua Zheng. Coded by Mengmeng Ao. This is the descrpition
#'
#' This is the details
#'
#' @param X T*N stock return matrix
#' @param F T*f matrix of factor returns
#' @param sigma risk constraint
#' @param K number of folds for cross validation
#' @param screening process the stop o screening process
#' @param Num.Screen number of stocks after screening
#' @param given.picks if a certain set of picked stocks is going to be used, set "given.picks" to be a numeric vector containing the indexes of the picked stocks (in terms of column numbers)
#' @return list of informatio containging weights
#'
#'
#' Notes:
#' 1. Setting "screening=TRUE" will trigger the Step 0 screening process (described in the paper), and Num.Screen is the number of stocks after screening; if a certain set of picked stocks is going to be used, set "given.picks" to be a numeric vector containing the indexes of the picked stocks (in terms of column numbers)
#' 2. If F is not specified (default setting), MAXSER portfolio weight without factor investing ("Scenario I" in the paper) will be computed. As long as F is specified, factor investing will be included in MAXSER strategy.
#'
#'
#' @importFrom zoo coredata
#' @importFrom lars lars
#' @importFrom lars predict.lars
#' @export MAXSER_weight


MAXSER_weight_CV_Constraint <- function(X, F=NULL, screening=FALSE, Num.Screen=50, sigma, K=10, given.picks=NULL, given.theta=NULL, est='theta-all', thres=NULL, risk.adj=FALSE)
{

  # For direct use of the codes here, package "lars" must be installed in R for computing lasso solutions; alternative packages such as "glmnet" can also be used with proper modification of the lasso function
  # library(lars)
  # source(SR.est.R)


  # ----------------------------------------------------handling exceptions----------------------------------------------------#
  if (!inherits(X, c('matrix', 'data.frame', 'xts'))) stop('X should be a matrix, data.frame or xts object')
  if (!is.null(given.picks) & !inherits(given.picks, 'numeric')) stop('given.picks should be given as a numerical vector')
  if (sigma < 0) warning('the risk level is negetive')
  if (K < 0) {
    K <- 10
    warning('The number of folds is negative, set K = 10')
  }

  if (inherits(X, 'data.frame')){
    X <- as.matrix(X)
  }else if (inherits(X, 'xts')){
    X <- coredata(X)
  }else
    X <- as.matrix(X)

  if (!screening & dim(X)[1] <= dim(X)[2]) stop('If not screening, the sample number should be larger than the number of dimension')
  if (screening & dim(X)[1] <= Num.Screen) stop('Choosing screening, the sample number should be larger than the number of screening')
  if (screening & dim(X)[2] <= Num.Screen) stop('Number of screening should be smaller than the number of screening')
  if (dim(X)[2] == 1) stop('Number of column should be greater than 1')
  col.na <- apply(X, 2, function(x) all(is.na(x)))
  if (any(col.na)) stop('Na with complete column: ', which(col.na == TRUE)[1])
  rng.diff <- apply(X, 2, function(x) diff(range(x)) == 0)
  if (any(rng.diff)) stop('constant column: ', which(rng.diff== TRUE)[1])
  # ---------------------------------------------------------------------------------------------------------------------------#


  # -------------------------------------------Scenario I: without factor investing------------------------------------------- #
  if(is.null(F) == TRUE){
    T <- dim(X)[1]
    N <- dim(X)[2]

    picks <- 0
    if(screening == TRUE){
      random.pick <- matrix(0, nr=1000, nc=Num.Screen)
      random.theta <- rep(0, 1000)
      for(g in 1:1000){
        random.pick[g,] <- sample(1:N, Num.Screen)
        random.data <- X[, random.pick[g,]]
        random.theta[g] <- SR.est(random.data)$theta.a
      }

      loc <- order(random.theta)[950]
      picks <- random.pick[loc, ]

      if(is.null(given.picks) == FALSE)
        picks <- given.picks

      X <- X[, picks]
    }

    if (is.null(given.theta)){
      theta <- (SR.est(X)$theta.a)[1]
    }else
      theta <- given.theta

    r_hat <- sqrt(theta)
    r_c <- (1+theta) / sqrt(theta) * sigma
    Y <- rep(r_c, T)

    all.folds <- split(sample(1:T), rep(1:K, length = T))

    lars.est <- lars(X, Y, type="lar", normalize=FALSE, intercept=FALSE)
    num.solu <- dim(coef(lars.est))[1]
    residmat <- matrix(0, num.solu, K)
    zeta<-rep(0, K)

    for(i in seq(K)) {
      omit <- all.folds[[i]]
      fit <- lars(X[ - omit,,drop=FALSE  ], Y[ - omit], type="lar", normalize=FALSE, intercept=FALSE)
      w <- coef(fit)  ###length(lambda)xp
      outfit <- X[omit, ] %*% t(w)
      if(length(omit) == 1)
        outfit <- matrix(outfit, nrow=1)
      residmat[, i] <- abs(sigma-apply(outfit, 2, sd))
      zeta[i] <- sum(abs(w[which.min(residmat[,i]), ])) / sum(abs(w[dim(w)[1], ]))
      # zeta[i] <- sum(abs(w[which.min(abs(sigma-apply(outfit, 2, sd))), ])) / sum(abs(w[dim(w)[1],]))
    }
    zeta.star <- mean(zeta)
    w.maxser <- matrix(predict.lars(lars.est, type="coefficients", mode="fraction", s=zeta.star)$coef)

    if(screening == TRUE){
      temp <- w.maxser
      w.maxser <- matrix(0, nr=p, nc=1)
      w.maxser[picks, ] <- temp
    }

    outlist <- list(picks=picks, w.maxser=w.maxser, theta=theta, zeta.star=zeta.star)
  }
  # ---------------------------------------------------------------------------------------------------------------------------#


  # --------------------------------------------Scenario II: with factor investing-------------------------------------------- #
  if(is.null(F) == FALSE){

    if (!inherits(F, c('matrix', 'data.frame', 'xts', 'numeric')))
      stop('F should be a matrix, data.frame, numeric or xts object')

    if (inherits(F, 'data.frame')){
      F <- as.matrix(F)
    }else if (inherits(F, 'xts')){
      F <- coredata(F)
    }else if (inherits(F, 'numeric')){
      F <- matrix(F, ncol = 1)
    }else
      F <- as.matrix(F)

    if (dim(F)[1] <= dim(F)[2]) stop('The sample number should be larger than the number of dimension')
    if (dim(F)[1] != dim(X)[1]) stop('number of observation in X is not equal to the number of observation in F')
    if (any(is.na(F))) warning('NA in F')
    col.na <- apply(F, 2, function(x) all(is.na(x)))
    if (any(col.na)) stop('Na with complete column: ', which(col.na == TRUE)[1])
    rng.diff <- apply(F , 2, function(x) diff(range(x)) == 0)
    if (any(rng.diff)) stop('constant column: ', which(rng.diff== TRUE)[1])


    T <- dim(X)[1]
    N <- dim(X)[2]
    f <- dim(F)[2]
    Beta <- matrix(0, nr=N, nc=f)
    Alpha <- matrix(0, nr=N, nc=1)
    picks <- 0

    for(i in 1:N) {
      fit <- lm(X[,i]~F)
      Beta[i,] <- fit$coef[-1]
      Alpha[i] <- fit$coef[1]
    }
    E <- X - F %*% t(Beta)
    Sigma.e <- cov(E)
    # Omega.e <- solve(Sigma.e)

    if(screening == TRUE){
      random.pick <- matrix(0, nr=1000, nc=Num.Screen)
      random.theta <- rep(0, 1000)
      for(g in 1:1000){
        random.pick[g, ] <- sample(1:N, Num.Screen)
        random.data <- X[, random.pick[g, ]]
        est <- SR.est(cbind(random.data, F))
        random.theta[g] <- est$theta.a
      }

      loc <- order(random.theta)[950]
      picks <- random.pick[loc, ]

      if(is.null(given.picks) == FALSE)
        picks <- given.picks

      X <- X[,picks]
      full.E <- E
      E <- E[,picks]
      full.Beta <- Beta
      Beta <- Beta[picks, ]
    }

    mu.f <- colMeans(F)
    Sigma.f <- cov(F)
    theta.f <- (t(mu.f) %*% solve(Sigma.f) %*% matrix(mu.f))[1]

    if (is.null(given.theta)) {
      if (est == 'theta-all'){
        theta <- (SR.est(cbind(X, F))$theta.a)[1]
        # theta <- (SR.est(cbind(X,F))$theta.sample)[1]
        theta.u <- theta - theta.f
      }else if (est == 'theta-u'){
        theta.u <- (SR.est(E)$theta.a)[1]
        theta <- theta.u + theta.f
      }
    } else {
      theta.u <- given.theta
      theta <- theta.u + theta.f
    }

    leverage <- 1
    
    if(theta.u>0){
      x.hat <- theta.u / theta
      r_hat <- sqrt(theta.u)
      r_c <- (1+theta.u) / sqrt(theta.u)
      Y <- rep(r_c, T)
      lars.est <- lars(E, Y, type="lar", normalize=FALSE, intercept=FALSE)
      all.folds <- split(sample(1:T), rep(1:K, length=T))
      num.solu <- dim(coef(lars.est))[1]
      residmat <- matrix(0, num.solu, K)
      zeta <- rep(0, K)

      #*****************************************#
      CV.risk <- rep(0, K)
      #*****************************************#

      for(i in seq(K)) {
        omit <- all.folds[[i]]
        fit <- lars(E[-omit,,drop=FALSE], Y[-omit], type="lar", normalize=FALSE, intercept=FALSE)
        w.ucv <- (sqrt(x.hat) * sigma * coef(fit))
        w.fcv <- matrix(0, nr=dim(w.ucv)[1], nc=f)

        if(x.hat != 1){
          for(s in 1:(dim(w.ucv)[1]))
            w.fcv[s,] <- sigma / sqrt(theta) * solve(Sigma.f) %*% mu.f-t(Beta) %*% matrix(w.ucv[s,])
        }else if(x.hat == 1){
          for(s in 1:(dim(w.ucv)[1]))
            w.fcv[s,] <- (-1) * t(Beta) %*% matrix(w.ucv[s,])
        }

        outfit <- X[omit, ] %*% t(w.ucv) + F[omit, ] %*% t(w.fcv)
        if(length(omit) == 1)
          outfit <- matrix(outfit, nrow=1)

        #*******************************************************************#
        satisfied.idx <- which(w.fcv > thres)
        temp_risk <- apply(outfit, 2, sd)
        if (length(satisfied.idx) != 0)
          temp_risk[-satisfied.idx] <- Inf
        temp_diff <- abs(sigma - temp_risk)
        #*******************************************************************#

        residmat[, i] <- temp_diff
        frac <- rowSums(abs(coef(fit))) / sum(abs(coef(fit)[num.solu, ]))
        zeta[i] <- frac[which.min(residmat[,i])]

        #*****************************************************#
        CV.risk[i] <- temp_risk[which.min(residmat[,i])]
        #*****************************************************#
      }

      #*****************************************************#
      CV.risk.star <- mean(CV.risk)
      leverage <- sigma / CV.risk.star
      #*****************************************************#

      zeta.star <- mean(zeta)
      w.A <- sqrt(x.hat) * sigma * predict(lars.est, type="coefficient", mode="fraction", s=zeta.star)$coef
      w_coef <- predict(lars.est, type="coefficient", mode="fraction", s=zeta.star)$coef
      l1norm <- sum(abs(w_coef))
      rmse <- sqrt(mean((Y-E%*%w_coef)^2))
    }

    if(theta.u<0 | theta.u==0){
      x.hat <- 0
      theta.u <- 0
      w.A <- matrix(0, nr=N, nc=1)
      if(screening==TRUE)
        w.A <- matrix(0, nr=Num.Screen, nc=1)
      rmse <- 0
      lars.est <- NULL
      r_c <- NA
      zeta.star <- 0
      l1norm <- 0
    }

    w.F <- sigma / sqrt(theta.f+theta.u) * solve(Sigma.f) %*% mu.f - t(Beta) %*% w.A
    if(x.hat == 1)
      w.F <- (-1) * t(Beta) %*% w.A

    if(screening == TRUE){
      temp <- w.A
      w.A <- matrix(0, nr=N, nc=1)
      w.A[picks,] <- temp
      Beta <- full.Beta
      E <- full.E
    }

    #*************************#
    if (risk.adj){
      w.A <- w.A * leverage
      w.F <- w.F * leverage
    }
    #*************************#

    w.maxser <- matrix(c(w.A, w.F))
    # ---------------------------------------------------------------------------------------------------------------------------#


    outlist <- list(lars.est=lars.est, zeta.star=zeta.star, l1norm=l1norm,
                    picks=picks, w.maxser=w.maxser, w.F=w.F, w.A=w.A,
                    x.hat=x.hat, theta=theta, theta.f=theta.f, theta.u=theta.u,
                    Beta=Beta, Alpha=Alpha, E=E, rmse=rmse, r_c=r_c, leverage=leverage)
  }

  return(outlist)
}
