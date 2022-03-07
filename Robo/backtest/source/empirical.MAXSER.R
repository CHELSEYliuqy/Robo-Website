empirical.MAXSER<-function(n=120,p=NULL,pure.random=FALSE,theta.loc=950, t=1,m=12, sigma=1, period, dataset, index.name=NULL, rf.name="rf_monthly_all_2017", factors.name=NULL, adjust.fac=FALSE, stock.only=FALSE, constituents.type="current", freq="monthly", screening=FALSE, Num.Screen=50, cv.method="fraction",theta.u.adj=FALSE,  thresh_1=0, thresh_2=100, SaveResult=FALSE, result.name)

################################# Version & Copyright Info ##################################################
###updated consistently with latest MAXSER, July 2018
###including several benchmark portfolios
###@copyright: Mengmeng Ao

################################# Parameter Interpretation ###################################################
### n: sample size (number of observations in each training period)
### p: (optional) number of stocks to choose from the whole stock universe, need to specify if stock pools smaller than the stock universe need to be formed
### pure.random: (optional) If TRUE, p stocks are randomly chosen from the stock universe each time of forming stock pools
### theta.loc: (optional) used when pure.random=FALSE and smaller stock pools are formed. The value of theta.loc range from 1 to 1000, corresponding to theta.loc/10 percentile. Default value 950 means the stock pool offering 95% quantile of all \hat{\theta}_{all} is chosen each time of forming stock pool
### t: length of holding period for 1 portfolio construction (i.e., window size)
### m: the length of period using 1 stock pool, which must be a multiplication of the value of t
### sigma: risk constraint level
### period: a character string that specifies the whole study period with standard format of date object, e.g., "1992-01-01/2016-12-31"
### dataset: a character string giving the name of stock (arithmetic) return data file
### index.name: a character string giving the name of stock index (arithmetic) return data file
### rf.name: a character string giving the name of risk-free rate data file
### factors.name: (optional) a character string giving the name of factors (arithmetic) returns data file if factor investing is allowed
### stock.only: If TRUE, only stocks are considered for all methods; if FALSE, factor investing is allowed for all methods and factors.name must be specified
### constituents.type: (optional) a character string giving the type of stock universe if index.status (to be explained below) is provided in dataset. "current" (default) means only the constituents at the time of portfolio construction are considered; "stay" means only the stocks remaining in the index for the training period and also at the time of portfolio construction are considered; "historical" means all the stocks that have been included in the index till the time of portfolio construction are considered
### freq: the frequency of return data, can be "monthly","weekly","bi-weekly","daily"
### thresh_1, thresh_2: inputs for MAXSER with factor investing. If theta.all<thresh_1*theta.f, theta.u is set to be 0 and only factors are invested; if theta.all>thresh_2*theta.f, theta.f is set to be 0 and only stocks are invested
### SaveResult, result.name: if SaveResult=TRUE, a file with name specified by result.name (a character string) is saved, containing all results; otherwise only a list is returned by the function
### cv.method: options are "step", "fraction", "norm"

############################### Data Input Description ######################################################
### stock returns (dataset) and index returns (index.name) can be prepared using another function "price2ret.R", which transforms raw price data into arithmetic returns and save them in the Rdata format required here. The required format of the csv file containing price data is described in price2ret.R.
### If the history of index constituents is available, dataset should also include a data frame named "index.status" containing 2 variables, "from" and "thru", that give the valid period of the constituents in the same order of the stocks as in stock return matrix.
### data file rf.name contains 1 xts object "rfall", which is the time series of risk-free rate
### data file factors.name contains 1 xts object "fac", in which each column gives the return series of one factor
### It should be guaranteed that the time length of stock, index, factors, and risk-free returns are the same for the specified study period

### Available datasets prepared by the author are listed in "Valid Empirical Dataset List.txt"


{
  library(xts)
  library(lars)
  source("weight.MAXSER.R")
  source("weight.maxser.nofac.R")
  source("weight.MAXSER.H.nonfac.R")
  source("weight.Kan.R")
  source("weight.NSS.R")
  source("weight.NSSL1.R")
  source("SR.test.R")
  source("SR.est.R")
  library(nlshrink)
  library(expm)
  library(quadprog)


  load(dataset)
  if(is.null(index.name)==FALSE) load(index.name)
  load(rf.name)


    ####subtract data during the study period
    data<-data[period]
    rf<-rfall[period]
    if(is.null(index.name)==FALSE) index<-index[period]
    if(length(rf)>dim(data)[1]) rf<-rf[-(1:(length(rf)-dim(data)[1]))]
    date.all<-time(data)
    data<-as.matrix(data)

    if(is.null(factors.name)==FALSE) {
      load(factors.name)
      fac<-as.matrix(fac[period])
      if(adjust.fac==TRUE) fac[,2:3]<-fac[,2:3]-0.0005
    }

   data<-data-matrix(rf, nr=length(rf), nc=dim(data)[2]) ###stock excess returns
   if(is.null(index.name)==FALSE) index<-as.matrix(index)-as.matrix(rf)[(length(rf)-length(index)+1):length(rf),1]

   if(stock.only==FALSE) f<-dim(fac)[2]
   if(stock.only==TRUE) f<-0

   NumP<-(length(date.all)-n)/t    ###number of portfolios to be constructed

   W.list<-vector("list", NumP)
   Asset.list<-vector("list",NumP)
   out.ret.list<-vector("list",NumP)
   full.out.ret<-vector("list",NumP)

   if(stock.only==TRUE)
     #methods.name<-c("Index","Equally weighted","MV-P","MV-LS", "GMV-LS","MV-P-NSS","MV-LS-NSS","MV-NLS-NSS","UC-NSS", "MAXSER","UC-NSS-L1")
     methods.name<-c("Index","Equally weighted","MV-P","MV-LS", "GMV-LS", "MAXSER", "MAXSER-H", "UC-NSS", "UC-NSS-L1")
   if(stock.only==FALSE)
     #methods.name<-c("Index","Equally weighted", "Factor","MV-P","MV-LS","GMV-LS","MV-P-NSS","MV-LS-NSS","MV-NLS-NSS","UC-NSS", "MAXSER", "UC-NSS-L1")
    methods.name<-c("Index","Equally weighted","Factor", "MV-P","MV-LS", "GMV-LS", "MAXSER", "UC-NSS", "UC-NSS-L1")

   return.all<-matrix(0,nr=length(date.all)-n,nc=length(methods.name))
   return.fullew<-matrix(0,nr=length(date.all)-n,nc=1)
   return.stockew<-matrix(0,nr=length(date.all)-n,nc=1)
   colnames(return.all)<-methods.name



  re.list<-vector("list",NumP)
  h.list<-vector("list", NumP)

  for(i in 1:NumP){

    if(stock.only==FALSE){
      M.X<-as.matrix(fac[((i-1)*t+1):((i-1)*t+n),])   ###factor training data
      M.Y<-as.matrix(fac[((i-1)*t+n+1):(i*t+n),])     ###factor testing data
    }


      ###first decide if it is the time to change stock basket
    if(((i-1)*t)%%m==0){
        #####this means portfolios have been constructed using the same basket for m months
        t0<-date.all[(i-1)*t+n]   ##current time

        if(exists("index.status")==TRUE){
          if(constituents.type=="current"){
            pool<-which(index.status$from <= t0 & index.status$thru >= t0 & apply(data[((i-1)*t+1):((i-1)*t+n+1),],2,min)>-9900)
          }

          if(constituents.type=="stay"){
            t_n<-date.all[(i-1)*t+1]
            pool<-which(index.status$from <= t_n & index.status$thru >= t0 & apply(data[((i-1)*t+1):((i-1)*t+n+1),],2,min)>-9900)
          }

          if(constituents.type=="historical"){
            pool<-which(index.status$from <= t0 & apply(data[((i-1)*t+1):((i-1)*t+n+1),],2,min)>-9900)
          }
        }

        if(exists("index.status")==FALSE){
          pool<-which(apply(data[((i-1)*t+1):((i-1)*t+n+1),],2,min)>-9900)
        }



        if(is.null(p)==TRUE) picked<-pool
        if(is.null(p)==FALSE)
        {
          if(pure.random==TRUE) {
            if(length(pool)>p) picked<-sample(pool, p)
            if(length(pool)<p) picked<-pool
          }
          if(pure.random==FALSE){
            random.pick<-matrix(0, nr=1000, nc=p)
            random.theta<-rep(0, 1000)
            mu.f<-colMeans(M.X)
            Sigma.f<-cov(M.X)
            theta.f<-(t(mu.f)%*%solve(Sigma.f)%*%matrix(mu.f))[1]
            for(g in 1:1000) {
              random.pick[g,]<-sample(pool, p)
              random.data<-as.matrix(data)[((i-1)*t+1):((i-1)*t+n), random.pick[g,]]
              est<-SR.est(cbind(random.data, M.X))
              random.theta[g]<-est$theta.a
              if(random.theta[g]<theta.f) random.theta[g]<-theta.f
            }
            theta.max<-max(random.theta)
            loc<-order(random.theta)[theta.loc]
            picked<-random.pick[loc, ]
          }
        }

    }

    X<-data[((i-1)*t+1):((i-1)*t+n), picked]     ###training stock returns
    Y<-data[((i-1)*t+n+1):(i*t+n),picked]        ###testing stock returns

    Asset.list[[i]]<-colnames(X)      ###stock names in the current pool

    w_even<-matrix(rep(1/(dim(X)[2]+f), dim(X)[2]+f))

    #### when all stocks in the training period have complete data
    if(min(X)>(-9900)){
      if(stock.only==FALSE){

        re.Fac<-weight.MAXSER(X=X, F=M.X, sigma=sigma, theta.u.adj=theta.u.adj, screening=screening, Num.Screen=Num.Screen, thresh_1= thresh_1, thresh_2=thresh_2, method = cv.method)
        w.A<-re.Fac$w.A
        w.Fac<-c(re.Fac$w.F)
        w.maxser<-c(w.A, w.Fac)
        re.list[[i]]<-re.Fac

        xbar<-matrix(colMeans(cbind(X,M.X)))
        S<-cov(cbind(X,M.X))

        k<-dim(cbind(X,M.X))[2]
        rho_oas<-min(((sum(diag(S%*%S))*(1-2/k)+(sum(diag(S)))^2)/((sum(diag(S%*%S))-(sum(diag(S)))^2/k)*(n-2/k))),1)
        S_ls<-rho_oas*sum(diag(S))/k*diag(1,k)+(1-rho_oas)*S

        theta.p<-(t(xbar)%*%solve(S)%*%xbar)[1]
        theta.ls<-(t(xbar)%*%solve(S_ls)%*%xbar)[1]

        w.p<-matrix(sigma/sqrt(theta.p)*solve(S)%*%xbar)
        w.ls<-matrix(sigma/sqrt(theta.ls)*solve(S_ls)%*%xbar)
        one<-matrix(1, nr=length(w.p), nc=1)
        w.gmv.ls<-solve(S_ls)%*%one/(t(one)%*%solve(S_ls)%*%one)[1]

        #re.nss<-weight.NSS(cbind(X,M.X), sigma=sigma)
        #w.nss<-re.nss$w.optimize

        re.nssl1<-weight.NSSL1(cbind(X,M.X), sigma=sigma)
        w.nssl1<-re.nssl1$w.NSSL1
        w.ucnss<-re.nssl1$w.ucnss
      }

      if(stock.only==TRUE){
        xbar<-matrix(colMeans(X))
        S<-cov(X)


        k<-dim(X)[2]
        rho_oas<-min(((sum(diag(S%*%S))*(1-2/k)+(sum(diag(S)))^2)/((sum(diag(S%*%S))-(sum(diag(S)))^2/k)*(n-2/k))),1)
        S_ls<-rho_oas*sum(diag(S))/k*diag(1,k)+(1-rho_oas)*S

        theta.p<-(t(xbar)%*%solve(S)%*%xbar)[1]
        theta.ls<-(t(xbar)%*%solve(S_ls)%*%xbar)[1]


        w.p<-matrix(sigma/sqrt(theta.p)*solve(S)%*%xbar)
        w.ls<-matrix(sigma/sqrt(theta.ls)*solve(S_ls)%*%xbar)
        one<-matrix(1, nr=k, nc=1)
        w.gmv.ls<-solve(S_ls)%*%one/(t(one)%*%solve(S_ls)%*%one)[1]

        re.list[[i]]<-weight.maxser.nofac(X=X, sigma=sigma, method = cv.method, screening=screening, Num.Screen=Num.Screen)
        w.maxser<-matrix(re.list[[i]]$w.maxser.nofac)

        h.list[[i]]<-weight.MAXSER.H.nonfac(X=X, sigma=sigma, K=10, method=cv.method)
        w.maxser.h<-matrix(h.list[[i]]$w.hat)

        #re.nss<-weight.NSS(X, sigma=sigma)
        #w.nss<-re.nss$w.optimize

        re.nssl1<-weight.NSSL1(X, sigma=sigma)
        w.nssl1<-re.nssl1$w.NSSL1
        w.ucnss<-re.nssl1$w.ucnss
      }
    }

    #### when there is missing data in the training period (this is possible when t < m )
    if(min(X)<(-9900)){
      non.trade<-which(apply(X,2,min)<(-9900))
      pp<-dim(X)[2]+f

      X<-X[,-non.trade]

      if(stock.only==FALSE){
        w_even<-matrix(1/(pp-length(non.trade)), nr=pp, nc=1)
        w_even[non.trade]<-0
        w.p<-matrix(0, nr=pp, nc=1)
        w.ls<-w.p
        w.gmv.ls<-w.p
        w.nssl1<-w.p
        w.ucnss<-w.p
        #w.nss<-matrix(0, nr=pp, nc=4)

        re.Fac<-weight.MAXSER(X=X, F=M.X, sigma=sigma,theta.u.adj=theta.u.adj, screening=screening, Num.Screen=Num.Screen, thresh_1= thresh_1, thresh_2=thresh_2, method = cv.method)
        w.Fac<-c(re.Fac$w.F)
        w.A<-re.Fac$w.A
        w.maxser<-matrix(0, nr=pp, nc=1)
        w.maxser[-non.trade]<-c(w.A, w.Fac)
        w.maxser[non.trade]<-0
        re.list[[i]]<-re.Fac

        xbar<-matrix(colMeans(cbind(X,M.X)))
        S<-cov(cbind(X,M.X))
        k<-dim(cbind(X,M.X))[2]
        rho_oas<-min(((sum(diag(S%*%S))*(1-2/k)+(sum(diag(S)))^2)/((sum(diag(S%*%S))-(sum(diag(S)))^2/k)*(n-2/k))),1)
        S_ls<-rho_oas*sum(diag(S))/k*diag(1,k)+(1-rho_oas)*S

        theta.p<-(t(xbar)%*%solve(S)%*%xbar)[1]
        theta.ls<-(t(xbar)%*%solve(S_ls)%*%xbar)[1]

        w.p[-non.trade]<-matrix(sigma/sqrt(theta.p)*solve(S)%*%xbar)
        w.ls[-non.trade]<-matrix(sigma/sqrt(theta.ls)*solve(S_ls)%*%xbar)
        one<-matrix(1, nr=k, nc=1)
        w.gmv.ls[-non.trade]<-solve(S_ls)%*%one/(t(one)%*%solve(S_ls)%*%one)[1]

        #re.nss<-weight.NSS(cbind(X, M.X), sigma=sigma)
        #w.nss[-non.trade,]<-re.nss$w.optimize

        re.nssl1<-weight.NSSL1(cbind(X,M.X), sigma=sigma)
        w.nssl1[-non.trade]<-re.nssl1$w.NSSL1
        w.ucnss[-non.trade]<-re.nssl1$w.ucnss
      }

      if(stock.only==TRUE){
        w_even<-matrix(1/(pp-length(non.trade)), nr=pp, nc=1)
        w_even[non.trade]<-0
        w.p<-matrix(0, nr=pp, nc=1)
        w.ls<-w.p
        w.gmv.ls<-w.p
        w.nssl1<-w.p
        w.ucnss<-w.p
        #w.nss<-matrix(0, nr=pp, nc=4)

        re.list[[i]]<-weight.maxser.nofac(X=X, sigma=sigma, method = cv.method, screening=screening, Num.Screen=Num.Screen)
        w.maxser<-matrix(0, nr=pp, nc=1)
        w.maxser[-non.trade]<-c(re.list[[i]]$w.maxser.nofac)
        w.maxser[non.trade]<-0

        h.list[[i]]<-weight.MAXSER.H.nonfac(X=X, sigma=sigma, K=10, method=cv.method)
        w.maxser.h<-matrix(0, nr=pp, nc=1)
        w.maxser.h[-non.trade]<-h.list[[i]]$w.hat
        w.maxser.h[non.trade]<-0

        xbar<-matrix(colMeans(X))
        S<-cov(X)

        k<-dim(X)[2]

        rho_oas<-min(((sum(diag(S%*%S))*(1-2/k)+(sum(diag(S)))^2)/((sum(diag(S%*%S))-(sum(diag(S)))^2/k)*(n-2/k))),1)
        S_ls<-rho_oas*sum(diag(S))/k*diag(1,k)+(1-rho_oas)*S

        theta.p<-(t(xbar)%*%solve(S)%*%xbar)[1]
        theta.ls<-(t(xbar)%*%solve(S_ls)%*%xbar)[1]

        w.p[-non.trade]<-matrix(sigma/sqrt(theta.p)*solve(S)%*%xbar)
        w.ls[-non.trade]<-matrix(sigma/sqrt(theta.ls)*solve(S_ls)%*%xbar)
        one<-matrix(1, nr=k, nc=1)
        w.gmv.ls[-non.trade]<-solve(S_ls)%*%one/(t(one)%*%solve(S_ls)%*%one)[1]

        #re.nss<-weight.NSS(X, sigma=sigma)
        #w.nss[-non.trade,]<-re.nss$w.optimize

        re.nssl1<-weight.NSSL1(X, sigma=sigma)
        w.nssl1[-non.trade]<-re.nssl1$w.NSSL1
        w.ucnss[-non.trade]<-re.nssl1$w.ucnss
      }
    }


    if(stock.only==TRUE)  W.list[[i]]<-cbind(w_even, w.p, w.ls,  w.gmv.ls, w.maxser,w.maxser.h, w.ucnss, w.nssl1)
    if(stock.only==FALSE){
     if(f==1) w_fac<-rbind(matrix(0, nr=length(w.p)-1, nc=1), sigma/sd(M.X))
     if(f>1){
       mu.f<-matrix(colMeans(M.X))
       cov.f<-cov(M.X)
       w_fac<-sigma/sqrt((t(mu.f)%*%solve(cov.f)%*%mu.f)[1])*solve(cov.f)%*%mu.f
       w_fac<-rbind(matrix(0, nr=length(w.p)-f, nc=1), w_fac)
     }

      W.list[[i]]<-cbind(w_even,w_fac, w.p, w.ls, w.gmv.ls, w.maxser,w.ucnss, w.nssl1)
    }

    if(t==1) {
      Y<-matrix(Y, nr=1, nc=length(Y))
      if(stock.only==FALSE) M.Y<-t(matrix(M.Y))
    }

    if(min(Y)<(-9900)){
      for(j in 1:nrow(Y)) Y[j, which(Y[j,]<(-9900))]<-0      ##equivalent to: if some stock is not traded at a testing point, all methods clear the position on it (the recorded weight are still the one based on training data)
    }

    if(stock.only==FALSE){
       out.ret.list[[i]]<-cbind(Y,M.Y)
       return.all[((i-1)*t+1):(i*t), -1]<-cbind(Y,M.Y)%*%W.list[[i]]
    }
    if(stock.only==TRUE){
       out.ret.list[[i]]<-Y
       return.all[((i-1)*t+1):(i*t), -1]<-Y%*%W.list[[i]]
    }

    W.list[[i]]<-matrix(W.list[[i]], nr=dim(W.list[[i]])[1], nc=dim(W.list[[i]])[2])
    colnames(W.list[[i]])<-methods.name[-1]

    full.out.ret[[i]]<-matrix(data[((i-1)*t+n+1):(i*t+n),pool], nr=t, nc=length(pool))
    if(min(full.out.ret[[i]])<(-9900)){
      for(j in 1:nrow(full.out.ret[[i]])) full.out.ret[[i]][j, which(full.out.ret[[i]][j,]<(-9900))]<-0
    }
    #return.stockew[((i-1)*t+1):(i*t)]<-rowMeans(full.out.ret[[i]])
    #return.fullew[((i-1)*t+1):(i*t)]<-rowMeans(cbind(full.out.ret[[i]], matrix(M.Y, nr=t, nc=f)))
  }

  if(is.null(index.name)==FALSE) return.all[,1]<-index[(length(index)-dim(return.all)[1]+1):length(index)]
  colnames(return.all)<-methods.name

  Return.Avg<-colMeans(return.all)
  Risk.Sd<-apply(return.all,2,sd)
  pval.max<-matrix(0, nr=length(methods.name), nc=1)
  pval.ucnss<-pval.max
  pval.nssl1<-pval.max
  for(s in 1:length(methods.name))
  {
    pval.max[s]<-SR.test(return.all[,"MAXSER"], return.all[,s])$pv
    pval.ucnss[s]<-SR.test(return.all[,"UC-NSS"], return.all[,s])$pv
    pval.nssl1[s]<-SR.test(return.all[,"UC-NSS-L1"], return.all[,s])$pv
  }
  if(freq=="monthly") SR<-Return.Avg/Risk.Sd*sqrt(12)
  if(freq=="biweekly") SR<-Return.Avg/Risk.Sd*sqrt(26)
  if(freq=="weekly") SR<-Return.Avg/Risk.Sd*sqrt(52)
  if(freq=="daily") SR<-Return.Avg/Risk.Sd*sqrt(252)

  result<-data.frame(Risk=Risk.Sd, Return=Return.Avg, Sharpe.ratio=SR, pvalue.max=pval.max, pvalue.ucnss=pval.ucnss,pval.nssl1=pval.nssl1, row.names=methods.name)
  print(round(result,4))

  if(SaveResult==TRUE) save(full.out.ret, return.stockew, return.fullew, W.list,re.list,out.ret.list, NumP, Asset.list, sigma, return.all, Return.Avg, Risk.Sd, SR, result,date.all, file=result.name)
  outlist<-list(index=index, sigma=sigma, W.list=W.list, NumP=NumP, Asset.list=Asset.list,date.all=date.all, re.list=re.list, return.all=return.all, Return.Avg=Return.Avg, Risk.Sd=Risk.Sd, SR=SR, result=result, out.ret.list=out.ret.list)
  return(outlist)
}
