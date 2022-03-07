# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

Equally_Weighted <- function(train_periods=NULL, pool=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))
  
  factor_name <- asset_return %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  asset_num <- dim(asset_return)[2] 
  asset_name <- asset_return %>% names()
  
  init_wts[, asset_name] <- as.list(rep(1/asset_num)) 
  wts <- init_wts %>% mutate_at(factor_name, as.factor)
  
  pb()
  
  return(wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

Factor <- function(train_periods=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  sigma <- bt_param$sigma
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(-Date)
  
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double)
  
  asset_name <- factor_return %>% names()
  asset_num <- dim(factor_return)[2]
  
  factor_name <- factor_return %>% names
  
  if (asset_num == 1){
    init_wts[, asset_name] <- sigma / factor_return %>% as.matrix() %>% sd
  }else{
    mu_f <- factor_return %>% colMeans()
    cov_f <- factor_return %>% cov()
    w_F <- sigma / sqrt((t(mu_f) %*% solve(cov_f) %*% mu_f)[1]) * solve(cov_f) %*% mu_f
    init_wts[, asset_name] <- as.list(w_F)
  }
  
  wts <- init_wts %>% mutate_at(factor_name, as.factor)
  
  pb()
  
  return(wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

MAXSER <- function(train_periods=NULL, pool=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  sigma <- bt_param$sigma
  est <- bt_param$est_method
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))
  
  asset_name <- asset_return %>% names()
  
  factor_name <- asset_pivot %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  stock_return <- asset_return %>% dplyr::select(!where(is.factor)) %>% as.matrix()
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double) %>% as.matrix()
  
  re_Fac <- MAXSER_weight(X=stock_return, F=factor_return, sigma=sigma, est=est)
  
  max_wts <- c(re_Fac$w.A, re_Fac$w.F) %>% as.numeric()
  if (any(is.na(max_wts))) max_wts = rep(0, length(max_wts))

  init_wts[, asset_name] <- max_wts %>% as.list()
  wts <- init_wts %>% mutate_at(factor_name, as.factor)
  
  pb()
  
  return(wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

MAXSER_Fac_Constraint <- function(train_periods=NULL, pool=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  sigma <- bt_param$sigma
  est <- bt_param$est_method
  fac_thres <- bt_param$fac_wts_constraint
  risk_adj <- bt_param$cv_risk_adj
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))
  
  asset_name <- asset_return %>% names()
  
  factor_name <- asset_pivot %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  stock_return <- asset_return %>% dplyr::select(!where(is.factor)) %>% as.matrix()
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double) %>% as.matrix()
  
  re_Fac <- MAXSER_weight_CV_Constraint(X=stock_return, F=factor_return, sigma=sigma, 
                                        thres=fac_thres, risk.adj=risk_adj, est=est)
  
  max_wts <- c(re_Fac$w.A, re_Fac$w.F) %>% as.numeric()
  if (any(is.na(max_wts))) max_wts = rep(0, length(max_wts))
  
  init_wts[, asset_name] <- max_wts %>% as.list()
  wts <- init_wts %>% mutate_at(factor_name, as.factor)

  pb()
  
  return(wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

MAXSER_Stock_Long_Only <- function(train_periods=NULL, pool=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  sigma <- bt_param$sigma
  est <- bt_param$est_method
  risk_adj <- bt_param$cv_risk_adj
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))
  
  asset_name <- asset_return %>% names()
  
  factor_name <- asset_pivot %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  stock_return <- asset_return %>% dplyr::select(!where(is.factor)) %>% as.matrix()
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double) %>% as.matrix()
  
  re_Fac <- MAXSER_weight_long_only(X=stock_return, F=factor_return, sigma=sigma, 
                                    risk.adj=risk_adj, est=est, core_num=10)
  
  max_wts <- c(re_Fac$w.A, re_Fac$w.F) %>% as.numeric()
  if (any(is.na(max_wts))) max_wts = rep(0, length(max_wts))
  
  init_wts[, asset_name] <- max_wts %>% as.list()
  wts <- init_wts %>% mutate_at(factor_name, as.factor)

  pb()
  
  return(wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

MAXSER_ALL_Long_Only <- function(train_periods=NULL, pool=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  sigma <- bt_param$sigma
  est <- bt_param$est_method
  risk_adj <- bt_param$cv_risk_adj
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))
  
  asset_name <- asset_return %>% names()
  
  factor_name <- asset_pivot %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  stock_return <- asset_return %>% dplyr::select(!where(is.factor)) %>% as.matrix()
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double) %>% as.matrix()
  
  re_Fac <- MAXSER_weight_factor_constraint(X=stock_return, F=factor_return, sigma=sigma,
                                            risk.adj=risk_adj, est=est, core_num=10)
  
  max_wts <- c(re_Fac$w.A, re_Fac$w.F) %>% as.numeric()
  if (any(is.na(max_wts))) max_wts = rep(0, length(max_wts))
  
  init_wts[, asset_name] <- max_wts %>% as.list()
  wts <- init_wts %>% mutate_at(factor_name, as.factor)

  pb()
  
  return(wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

MAXSER_H <- function(train_periods=NULL, pool=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  sigma = bt_param$sigma
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))

  asset_name <- asset_return %>% names()
  
  factor_name <- asset_pivot %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  stock_return <- asset_return %>% dplyr::select(!where(is.factor)) %>% as.matrix()
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double) %>% as.matrix()
  
  re_H <- MAXSER_HTV(X=stock_return, F=factor_return, sigma=sigma, gamma.predict.method="latest", freq="daily")
  
  max_wts <- c(re_H$w.A, re_H$w.F) %>% as.numeric()
  if (any(is.na(max_wts))) max_wts = rep(0, length(max_wts))
  
  init_wts[, asset_name] <- max_wts %>% as.list()
  wts <- init_wts %>% mutate_at(factor_name, as.factor)
  
  pb()
  
  return(wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

MAXSER_H_Stock_Long_Only <- function(train_periods=NULL, pool=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})

  sigma <- bt_param$sigma
  est <- bt_param$est_method
  risk_adj <- bt_param$cv_risk_adj

  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))
  
  asset_name <- asset_return %>% names()
  
  factor_name <- asset_pivot %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  stock_return <- asset_return %>% dplyr::select(!where(is.factor)) %>% as.matrix()
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double) %>% as.matrix()

  re_H <- MAXSER_HTV_long_only(X=stock_return, F=factor_return, sigma=sigma,
                               gamma.predict.method="latest", freq="daily", risk.adj=risk_adj)

  max_wts <- c(re_H$w.A, re_H$w.F) %>% as.numeric()
  if (any(is.na(max_wts))) max_wts = rep(0, length(max_wts))
  
  init_wts[, asset_name] <- max_wts %>% as.list()
  wts <- init_wts %>% mutate_at(factor_name, as.factor)

  pb()
  
  return(wts)
}
# ======================================================================================================== #





# ======================================================================================================== #
#' @param train_data_segment 
#' @param init_wts
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

MAXSER_H_ALL_Long_Only <- function(train_periods=NULL, pool=NULL, bt_param=NULL, asset_pivot=NULL, init_wts=NULL, pb){
  
  # browser(expr={debug})
  
  sigma <- bt_param$sigma
  est <- bt_param$est_method
  risk_adj <- bt_param$cv_risk_adj
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  
  asset_return <- asset_pivot %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% dplyr::select(all_of(pool))
  
  asset_name <- asset_return %>% names()
  
  factor_name <- asset_pivot %>% 
    dplyr::select(where(is.factor)) %>% names(.)
  
  stock_return <- asset_return %>% dplyr::select(!where(is.factor)) %>% as.matrix()
  factor_return <- asset_return %>% dplyr::select(where(is.factor)) %>% 
    mutate_if(is.factor, factor_to_double) %>% as.matrix()
  
  re_H <- MAXSER_HTV_factor_constraint(X=stock_return, F=factor_return, sigma=sigma, 
                                       gamma.predict.method="latest", freq="daily", 
                                       risk.adj=risk_adj, core_num=10)
  
  max_wts <- c(re_H$w.A, re_H$w.F) %>% as.numeric()
  if (any(is.na(max_wts))) max_wts = rep(0, length(max_wts))
  
  init_wts[, asset_name] <- max_wts %>% as.list()
  wts <- init_wts %>% mutate_at(factor_name, as.factor)

  pb()
  
  return(wts)
}
# ======================================================================================================== #
