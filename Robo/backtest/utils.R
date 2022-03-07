# ======================================================================================================== #
#' @param x 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

factor_to_double <- function(x) {
  
  return(as.numeric(levels(x))[x])
  
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param calendar 
#' @param bt_param
#' @param adj_margin
#' @param display 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

calendar_periods_segment <- function(calendar=NULL, bt_param=NULL, adj_margin=15, display=TRUE){
  
  # browser(expr={debug})
  
  test_start <- date(bt_param$test_start)
  test_end <- bt_param$test_end
  test_data_freq <- bt_param$calc_param$eval_freq_dates
  
  rwt_freq <- bt_param$calc_param$rwt_freq
  train_wd <- bt_param$calc_param$train_wd
  train_begin_lag <- bt_param$calc_param$train_begin_lag
  train_end_lag <- bt_param$calc_param$train_end_lag
  
  train_wd_freq <- train_wd %>% strsplit(., ' ') %>% unlist(.) %>% .[2]
  train_wd_len <- train_wd %>% strsplit(., ' ') %>% unlist(.) %>% .[1] %>% as.numeric(.)
  rwt_wd_freq <- rwt_freq %>% strsplit(., ' ') %>% unlist(.) %>% .[2]
  rwt_wd_len <- rwt_freq %>% strsplit(., ' ') %>% unlist(.) %>% .[1] %>% as.numeric(.)
  
  tblg <- 0
  telg <- 0
  if(!is.na(train_begin_lag)){
    tblg_freq <- train_begin_lag %>% strsplit(., ' ') %>% unlist(.) %>% .[2]
    tblg_len <- train_begin_lag %>% strsplit(., ' ') %>% unlist(.) %>% .[1] %>% as.numeric(.)
    tblg <- get(tblg_freq)(tblg_len)
  }
  
  if(!is.na(train_end_lag)){
    telg_freq <- train_end_lag %>% strsplit(., ' ') %>% unlist(.) %>% .[2]
    telg_len <- train_end_lag %>% strsplit(., ' ') %>% unlist(.) %>% .[1] %>% as.numeric(.)
    telg <- get(telg_freq)(telg_len)
  }
  
  rwt_lag_unit <- get(train_wd_freq)(train_wd_len) / get(rwt_wd_freq)(rwt_wd_len)
  
  if (is.na(test_end)){
    test_end <- 'end'
  }else{
    test_end <- date(test_end)
  }
  
  tb1 <- calendar[[test_data_freq]] %>%
    as_tbl_time(Date) %>% 
    filter_time(test_start-years(20) ~ test_end) %>% 
    collapse_by(rwt_freq, side='start') %>% unique() %>%
    rename(train_begin = Date) %>%
    mutate(train_begin = lag(train_begin, rwt_lag_unit)) %>% 
    mutate(join=1:dim(.)[1])
  
  tb2 <- calendar[[test_data_freq]] %>%
    as_tbl_time(Date) %>% 
    filter_time(test_start-years(20) ~ test_end) %>% 
    collapse_by(rwt_freq, side='end') %>% unique() %>%
    rename(test_end = Date) %>% 
    mutate(rwt_date = lag(test_end)) %>%
    unique() %>% mutate(join=1:dim(.)[1])
  
  periods_summary <- left_join(tb1, tb2) %>% dplyr::select(-join) %>%
    mutate(test_begin = rwt_date + 1) %>%
    mutate(train_end = rwt_date) %>%
    relocate(rwt_date, train_begin, train_end, test_begin, test_end) %>%
    as_tbl_time(test_begin) %>%
    filter_time(test_start-5 ~ 'end') %>% as_tibble()
  
  periods_summary <- periods_summary %>%
    mutate(train_begin = train_begin - tblg) %>%
    mutate(train_end = train_end - telg)
  
  nested <- periods_summary %>% group_by(rwt_date) %>% 
    nest(train_periods = c(train_begin, train_end)) %>% 
    nest(test_periods = c(test_begin, test_end)) %>% ungroup
  
  if (display)
    print(periods_summary)
  
  return(nested)
}
# ======================================================================================================== #



# ======================================================================================================== #
#' @param calendar 
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

volume_periods_segment <- function(calendar=NULL, bt_param=NULL, display=TRUE){
  
  # browser(expr={debug})
  
  test_start <- date(bt_param$test_start)
  test_end <- bt_param$test_end
  
  if (is.na(test_end)){
    test_end <- 'end'
  }else{
    test_end <- date(test_end)
  }
  
  test_start_volc <- calendar %>% as_tbl_time(Date) %>%
    filter_time(test_start ~ test_end) %>% as_tibble() %>% 
    .[['vol_clock']] %>% head(., 1)
  test_end_volc <- calendar %>% as_tbl_time(Date) %>%
    filter_time(test_start ~ test_end) %>% as_tibble() %>% 
    .[['vol_clock']] %>% tail(., 1)
  
  rwt_freq <- bt_param$volc_param$rwt_freq
  train_wd <- bt_param$volc_param$train_wd
  
  test_begin_volc <- seq(test_start_volc, test_end_volc, by=rwt_freq)
  train_begin_volc <- test_begin_volc - train_wd
  train_end_volc <- test_begin_volc - 1
  rwt_volc <- train_end_volc
  
  rwt_tb <- calendar %>% filter(vol_clock %in% rwt_volc) %>%
    dplyr::select(Date) %>% rename(rwt_date = Date)
  train_begin_tb <- calendar %>% filter(vol_clock %in% train_begin_volc) %>%
    dplyr::select(Date) %>% rename(train_begin = Date)
  train_end_tb <- calendar %>% filter(vol_clock %in% train_end_volc) %>%
    dplyr::select(Date) %>% rename(train_end = Date)
  test_begin_tb <- train_end_tb %>% transmute(test_begin = train_end + 1)
  test_end_tb <- rwt_tb %>% transmute(test_end = c(rwt_date[-1], calendar$Date[test_end_volc]))

  periods_summary <- cbind(rwt_tb, train_begin_tb, train_end_tb,
                           test_begin_tb, test_end_tb) %>% as_tibble
  
  nested <- periods_summary %>% group_by(rwt_date) %>% 
    nest(train_periods = c(train_begin, train_end)) %>% 
    nest(test_periods = c(test_begin, test_end))
  
  if (display)
    print(periods_summary)
  
  return(nested)
}
# ======================================================================================================== #



# ======================================================================================================== #
#' @param period_segment 
#' @param stock_rf_data
#' @param factor_rf_data 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

initialize_wts <- function(rwt_date=NULL, test_pivot=NULL, pb){
  
  # browser(expr={debug})
  
  init_wts <- test_pivot[1, ] %>% 
    mutate_all(function(x){x=0}) %>% mutate(Date = rwt_date)

  if (!is.null(pb))
    pb()
  
  return(init_wts)
}
# ======================================================================================================== #





# ======================================================================================================== #
#' @param train_data_segment 
#' @param index_info 
#' @param random_seed 
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

asset_pool_segment <- function(train_periods=NULL, stk=NULL, fct=NULL, index_info=NULL, random_seed=NULL, bt_param=NULL, pb){
  
  no_exit <- function(x){
    dd <- x$is_in
    return(any(as.data.frame(table(cumsum(dd)))[, 'Freq'][-1] > 1) == FALSE)
  }
  
  # browser(expr=(debug))
  
  train_begin <- train_periods$train_begin
  train_end <- train_periods$train_end
  rwt_date <- train_end
  
  stk_pool <- bt_param$stk_pool
  stk_pool_num <- bt_param$stk_pool_num
  
  stk_pot <- stk %>% as_tbl_time(Date) %>% 
    filter_time(train_begin ~ train_end) %>% as_tibble %>%
    mutate_at('rf_return', replace_na, -9900)
  
  in_td <- stk_pot %>% group_by(Code) %>%
    mutate(min_ret = min(rf_return)) %>% 
    filter(min_ret > -9900) %>% .$Code %>% unique
  
  if (is.na(stk_pool)){
    if (all(names(index_info) %in% names(stk))){
      in_idx <- stk_pot %>%
        filter(Date==rwt_date & type=='stock' & is_in == 1) %>% 
        .$Code %>% unique()
    }else if ('from' %in% names(index_info) | 'thru' %in% names(index_info)){
      in_idx <- index_info %>%
        filter(from <= rwt_date & thru >= rwt_date) %>% .$Code %>% unique()
    }else{
      in_idx <- in_td
    }
    
    temp_pool <- intersect(in_idx, in_td)
    
    if (is.na(bt_param$stk_screen_mtd)){
      stock_pool <- temp_pool
    }else{
      if (bt_param$stk_screen_mtd == 'no_exit'){
        current_status <- index_info %>% filter(Date <= rwt_date) %>%
          filter(Code %in% in_idx)
        valid_stocks <- current_status %>% group_by(Code) %>% nest() %>%
          transmute(valid = map(data, ~no_exit(.x))) %>% 
          unnest(valid) %>% filter(valid==TRUE) %>% .$Code
        stock_pool <- intersect(valid_stocks, in_td)
      }else if (bt_param$stk_screen_mtd == 'max_mkt_cap'){
        stock_pool <- stk_pot %>%
          filter(Date == rwt_date & Code %in% temp_pool) %>% 
          arrange(desc(mkt_cap)) %>% .[1:stk_pool_num, ] %>% .$Code
      }else if(bt_param$stk_screen_mtd == 'min_mkt_cap'){
        stock_pool <- train_data_segment %>%
          filter(Date == rwt_date & Code %in% temp_pool) %>% 
          arrange(mkt_cap) %>% .[1:stk_pool_num, ] %>% .$Code
      }else if (bt_param$stk_screen_mtd == 'pure_random'){
        a <- 1 #####
      }
    }
  }else{
    stock_pool <- intersect(in_td, stk_pool)
  }


  factor_pool <- fct$Code %>% unique
  asset_pool <- c(stock_pool, factor_pool)

  if (!is.null(pb))
    pb()
  
  return(asset_pool)
}
# for (i in 1:NumP){
#   F.train[[i]] <- train$factor[Dates[[i]]$train.period]
#   F.eval[[i]] <- eval$factor[Dates[[i]]$eval.period]
#   if (!is.null(trunc) & fac.num==1){
#     trunc.idx <- c(which(F.train[[i]] > quantile(F.train[[i]], 1-trunc)), which(F.train[[i]] < quantile(F.train[[i]], trunc)))
#     F.train[[i]] <- F.train[[i]][-trunc.idx]
#   }
#   
#   if (inherits(index.info, 'xts')){
#     constituent.stocks <- names(index.info[, which(index.info[Dates[[i]]$train.end]==1)])
#     if (stock.screen){
#       current.status <- index.info[paste0('::', Dates[[i]]$train.end)][, constituent.stocks]
#       selected.stocks <- names(which(apply(current.status, 2, function(x) any(as.data.frame(table(cumsum(x)))[, 'Freq'][-1]!=1))==FALSE))
#       data <- train$stock[Dates[[i]]$train.period][, selected.stocks]
#       pool <- names(which(apply(data[paste0('::', Dates[[i]]$test.begin)], 2, min)>-9900))
#     }else{
#       data <- train$stock[Dates[[i]]$train.period][, constituent.stocks]
#       pool <- names(which(apply(data[paste0('::', Dates[[i]]$test.begin)], 2, min)>-9900))
#     }
#   }else if(inherits(index.info, 'data.frame')){
#     pool <- names(which(as.Date(index.info$from) <=Dates[[i]]$train.end & as.Date(index.info$thru)>=Dates[[i]]$train.end & apply(train$stock[Dates[[i]]$train.period], 2, min)>-9900))
#   }else
#     pool <- names(which(apply(train$stock[paste0('::', Dates[[i]]$test.begin)], 2, min)>-9900))
#   
#   if (is.null(picks.num)){
#     picked <- pool
#   }else{
#     if(pure.random){
#       if (length(pool) > picks.num){
#         set.seed(random.seed[i])
#         picked <- sample(pool, picks.num)
#       }else
#         picked <- pool
#     }else if (!is.null(picks.id)){
#       # pool.id <- train[[picks.id]][Dates[[i]]$train.end][, pool]
#       # pool.id <- colMeans(train[[picks.id]][Dates[[i]]$train.period][, pool], na.rm=TRUE)
#       pool.id <- train[[picks.id]][Dates[[i]]$train.end][, pool]
#       picked <- names(pool.id)[order(pool.id, decreasing=decrease)[1:picks.num]]
#     }else{
#       random.pick <- matrix(0, nr=1000, nc=picks.num)
#       random.theta <- rep(0, 1000)
#       mu.f <- colMeans(F.train[[i]])
#       Sigma.f <- cov(F.train[[i]])
#       theta.f <- (t(mu.f)%*%solve(Sigma.f)%*%matrix(mu.f))[1]
#       for(g in 1:1000) {
#         random.pick[g, ] <- sample(pool, picks.num)
#         random.data <- train$stock[Dates[[i]]$train.period][,random.pick[g,]]
#         est <- SR.est(cbind(random.data, F.train[[i]]))
#         random.theta[g] <- est$theta.a
#         if(random.theta[g] < theta.f) 
#           random.theta[g]<-theta.f
#       }
#       theta.max <- max(random.theta)
#       loc <- order(random.theta)[theta.loc]
#       picked <- random.pick[loc, ]
#     }
#   }
#   
#   X.train[[i]] <- train$stock[Dates[[i]]$train.period][, picked]
#   X.eval[[i]] <- eval$stock[Dates[[i]]$eval.period]
#   
#   if (!is.null(trunc) & fac.num==1)
#     X.train[[i]] <- X.train[[i]][-trunc.idx, ]
#   
#   Asset.list[[i]] <- c(colnames(X.train[[i]]), colnames(F.train[[i]])) 
# }
# 
# return.comp <- list(X.train=X.train, X.eval=X.eval, 
#                     F.train=F.train, F.eval=F.eval,
#                     RF=RF, Asset.list=Asset.list,
#                     index.eval=Excess.Ret$eval$index,
#                     ETF.eval=Excess.Ret$eval$ETF)
# 
# return(return.comp)
# ======================================================================================================== #




# ======================================================================================================== #
#' @param wts_list 
#' @param daily_cal 
#' @param reb_freq 
#' @param pb 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

rebalance_weights <- function(wts_list=NULL, daily_cal=NULL, reb_freq=NULL, pb){
  
  # browser(expr=(debug))

  if (reb_freq != 'None'){
    reb_wts <- left_join(daily_cal, wts_list, by='Date') %>% 
      mutate_all(na.locf, na.rm=F, fromLast=F) %>% drop_na() %>% 
      as_tbl_time(Date) %>% as_period(reb_freq, side='end', 
      include_endpoints=TRUE) %>% as_tibble
  }else{
    reb_wts <- wts_list
  }

  if (!is.null(pb))
    pb()
  
  return(reb_wts)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param wts_list 
#' @param daily_cal 
#' @param reb_freq 
#' @param pb 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

wts_summary <- function(agg_wts, pb){

  stock_num_summary <- function(wts_segment){
  stk_wts <- wts_segment %>% dplyr::select(!where(is.factor)) %>% 
    dplyr::select(where(is.numeric))
  pos_num <- sum(stk_wts > 0)
  neg_num <- sum(stk_wts < 0)
  return(tibble(Date=wts_segment$tpl_date, 'Num +'=pos_num, 'Num -'=neg_num))
  }
  
  
  stock_value_summary <- function(wts_segment){
    num_summary = stock_num_summary(wts_segment)
    pos_num = num_summary$`Num +`
    neg_num = num_summary$`Num -`
    stk_wts = wts_segment %>% dplyr::select(!where(is.factor)) %>% 
      dplyr::select(where(is.numeric))
    if (pos_num == 0){
      pos_sum = 0
      pos_max = 0
    }else{
      pos_sum = sum(stk_wts[stk_wts > 0])
      pos_max = max(stk_wts[stk_wts > 0])
    }
    
    if (neg_num == 0){
      neg_sum = 0
      neg_min = 0
    }else{
      neg_sum = sum(stk_wts[stk_wts < 0])
      neg_min = min(stk_wts[stk_wts < 0])
    }
    
    return(tibble(Date=wts_segment$tpl_date, 
                  'Sum +'=pos_sum, 'Max +'=pos_max, 
                  'Min -'=neg_min, 'Sum -'=neg_sum))
  }
  
  factor_wts <- function(wts_segment){
    
    fac_wts = wts_segment %>% 
      dplyr::select(where(is.factor)) %>% 
      mutate_if(is.factor, factor_to_double) %>%
      mutate(Date = wts_segment$tpl_date) %>%
      relocate(Date)
    return(fac_wts)
  }

  # browser(expr=(debug))

  if (!all(is.null(agg_wts))){
    agg_wts <- agg_wts %>% mutate_if(is.numeric, replace_na, 0)
    agg_wts <- agg_wts %>% mutate_if(is.factor, function(x){x=factor_to_double(x); x[is.na(x)]=0; x=as.factor(x)})

    summary <- agg_wts %>% mutate(tpl_date=Date) %>% 
      relocate(tpl_date, .after=Date) %>% 
      group_by(Date) %>% nest() %>% ungroup %>%
      rename(wts_segment = data) %>%
      mutate(stk_num_summary = map(wts_segment, ~stock_num_summary(.x))) %>%
      mutate(stk_val_summary = map(wts_segment, ~stock_value_summary(.x))) %>%
      mutate(factor = map(wts_segment, ~factor_wts(.x))) %>% 
      dplyr::select(-Date, -wts_segment)
  }else{
    summary = NA
  }
  
  if (!is.null(pb))
    pb()
  
  return(summary)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param agg_return 
#' @param agg_BOP_wts 
#' @param agg_EOP_wts 
#' @param cost 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

transaction_cost <- function(agg_return=NULL, agg_BOP_wts=NULL, agg_EOP_wts=NULL, cost=NULL){
  # browser(expr=(debug))
  
  if (all(is.null(agg_BOP_wts)) & all(is.null(agg_EOP_wts))){
    agg_return_cost <- agg_return
  }else{
    EOP_wts <- agg_EOP_wts %>% dplyr::select(-Date) %>% lag %>% replace(is.na(.), 0)
    BOP_wts <- agg_BOP_wts %>% dplyr::select(-Date)
    position_alter <- (EOP_wts - BOP_wts) %>% 
      transmute(alter = 1 - rowSums(abs(.)) * cost$stock) %>%
      mutate(Date = agg_return$Date)
    
    agg_return_cost <- agg_return %>% 
      left_join(., position_alter, by='Date') %>%
      mutate(portfolio.returns = (1 + portfolio.returns) * alter - 1) %>%
      dplyr::select(-alter)
  }
  return(agg_return_cost)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param wts_list 
#' @param asset_pivot 
#' @param bt_param
#' @param pb
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

portfolio_info <- function(wts_list=NULL, asset_pivot=NULL, bt_param=NULL, pb){
  
  # browser(expr=(debug))
  
  cost <- bt_param$trans_cost
  
  date_range <- paste0(bt_param$test_start, '::', bt_param$test_end)
  
  xts_wts <- wts_list %>% mutate_if(is.factor, factor_to_double) %>% tbl_xts
  xts_ret <- asset_pivot %>% mutate_if(is.factor, factor_to_double) %>%
    mutate_if(is.numeric, function(x){x %>% replace_na(0)}) %>% tbl_xts
  
  xts_wts <- xts_wts[date_range]
  xts_ret <- xts_ret[paste0('::', bt_param$test_end)]
  
  port_info <- Return.portfolio(R=xts_ret, weights=xts_wts, contribution=TRUE, verbose=TRUE)
  port_info <- port_info %>% lapply(., xts_tbl) %>% 
    lapply(., function(x) {x %>% rename(. , Date=date)}) %>% 
    lapply(., list) %>% as_tibble() %>% 
    rename(Rf_Return=returns, BOP_Weight=BOP.Weight, EOP_Weight=EOP.Weight, BOP_Value=BOP.Value, EOP_Value=EOP.Value) %>%
    mutate(Rf_Return_Cost = pmap(list(Rf_Return, BOP_Weight, EOP_Weight), transaction_cost, cost=cost)) %>%
    relocate(Rf_Return_Cost, .after = Rf_Return)
  
  if (!is.null(pb))
    pb()
  
  return(port_info)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param agg_return 
#' @param risk_free 
#' @param first_rwt_date 
#' @param pb 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

risk_free_adjust <- function(agg_return=NULL, risk_free=NULL, first_rwt_date=NULL, pb=NULL){
  # browser(expr=(debug))
  
  if (!all(is.na(risk_free))){
    start <- head(agg_return$Date, 1)
    end <- tail(agg_return$Date, 1)
    risk_free <- risk_free %>% as_tbl_time(Date) %>%
      filter_time(start ~ end) %>% as_tibble
    agg_return_raw <- left_join(agg_return, risk_free, by='Date') %>% 
      transmute(Date=Date, portfolio.returns=portfolio.returns+RF)
  }else{
    agg_return_raw <- agg_return
  }
  
  first_day_return <- tibble(Date=first_rwt_date, portfolio.returns=0)
  agg_return_raw <- agg_return_raw %>% rbind(first_day_return, .)
  
  if (!is.null(pb))
    pb()
  
  return(agg_return_raw)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param agg_return 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

cumulative_return <- function(agg_return){
  # browser(expr=(debug))
  agg_cum_return <- agg_return %>%
    mutate_if(is.numeric, function(x) {cumprod(1+x)}) %>%
    rename(cumulative_returns=portfolio.returns)
  return(agg_cum_return)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param agg_return 
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

drawdown <- function(agg_return=NULL){
  dd <- agg_return %>% tbl_xts() %>%
    Drawdowns() %>% na.fill(0) %>% xts_tbl() %>% 
    rename(Date=date, drawdowns=portfolio.returns)
  return(dd)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param agg_rf_return 
#' @param agg_raw_return 
#' @param benchmark_return 
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

agg_statistics <- function(agg_rf_return=NULL, agg_raw_return=NULL, benchmark_return=NULL, bt_param=NULL, pb=NULL){
  
  # browser(expr=(debug))
  
  test_freq <- bt_param$calc_param$eval_freq
  # test_freq = 'monthly'
  if (test_freq == 'daily'){
    scale_ <- 252
  }else if (test_freq == 'weekly'){
    scale_ <- 52
  }else{
    scale_ <- 12
  }
  
  rf_ret <- agg_rf_return %>% tbl_xts()
  raw_ret <- agg_raw_return %>% tbl_xts()
  bcm_ret <- benchmark_return %>% tbl_xts()
  
  fit <- lm(agg_rf_return$portfolio.returns ~ benchmark_return$portfolio.returns)
  idiosyn_ret <- agg_rf_return$portfolio.returns - benchmark_return$portfolio.returns * fit$coef[2]
  
  Risk_Annual_Idio = sd(idiosyn_ret, na.rm=TRUE) * sqrt(scale_)
  if (abs(CAPM.alpha(rf_ret, bcm_ret)) - 1e-10 < 0){
    SR_Annual_Idio = 0
  }else{
    SR_Annual_Idio = CAPM.alpha(rf_ret, bcm_ret) * scale_ / Risk_Annual_Idio
  }
  
  agg_stat <- tibble(
    Sigma_Anuual = bt_param$sigma_annual,
    Risk_Annual = sd(rf_ret, na.rm=TRUE) * sqrt(scale_),
    Risk_Annual_Idio = Risk_Annual_Idio, 
    SR_Annual = mean(rf_ret, na.rm=TRUE) * scale_ / Risk_Annual, 
    SR_Annual_Idio = SR_Annual_Idio, 
    Return_Anuual = Return.annualized(raw_ret) %>% as.numeric, 
    Return_Cum = Return.cumulative(raw_ret) %>% as.numeric,
    MaxDD = maxDrawdown(raw_ret),
    Beta = CAPM.beta(rf_ret, bcm_ret),
    Alpha_Annual = CAPM.alpha(rf_ret, bcm_ret) * scale_
  ) %>% mutate_at(vars(-SR_Annual, -SR_Annual_Idio, -Beta), scales::percent, accuracy=1e-2)  %>% 
        mutate_at(vars(SR_Annual, SR_Annual_Idio, Beta), scales::number, accuracy=1e-3)

  if (!is.null(pb))
    pb()
  
  return(agg_stat)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param port_agg_sheet 
#' @param port_stat_sheet
#' @param save_file
#' @param smooth
#' @param port_slice   
#' @param bt_param
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

portfolio_value_plot <- function(port_info=NULL, port_stat=NULL, save_info=NULL, 
                                 smooth=FALSE, port_slice=NULL, bt_param=NULL, 
                                 time_sheet=NULL, daily_cal=NULL){
  
  # browser(expr=(debug))
  
  # cum_ret <- port_info %>% 
  #   dplyr::select(Portfolio, Cum_Return_Cost) %>% 
  #   filter(Portfolio %in% port_slice)  %>% unnest(Cum_Return_Cost)

  raw_ret <- port_info %>% 
    dplyr::select(Portfolio, Raw_Return_Cost) %>% 
    filter(Portfolio %in% port_slice)  %>% unnest(Raw_Return_Cost) %>% 
    filter(Date >= time_sheet$rwt_date[1]) %>% 
    mutate(portfolio.returns=if_else(Date==time_sheet$rwt_date[1],0, portfolio.returns)) 
  
  cum_ret <- raw_ret %>% group_by(Portfolio) %>% 
    mutate(cumulative_returns=cumprod(1+portfolio.returns)) %>% ungroup 

  # cum_ret <- cum_ret %>% mutate(Portfolio=if_else(Portfolio=='MAXSER_ALL_Long_Only_CSI500ETF-3years', 'MAXSER', Portfolio))  
  # cum_ret <- cum_ret %>% mutate(Portfolio=if_else(Portfolio=='MAXSER_H_ITOT-10years', 'MAXSER-H', Portfolio))  
  
  # dd <- port_info %>%
  #   dplyr::select(Portfolio, DrawDown) %>% 
  #   filter(Portfolio %in% port_slice) %>% unnest(DrawDown) 

  dd <- raw_ret %>% 
    pivot_wider(values_from = portfolio.returns, names_from = Portfolio) %>% 
    tbl_xts %>% Drawdowns() %>% na.fill(0) %>% xts_tbl() %>% 
    pivot_longer(!date, values_to = 'drawdowns', names_to = 'Portfolio') %>% rename(Date=date) 
  
  my_table <- port_stat %>% 
    filter(Portfolio %in% port_slice) %>% as.data.frame()
  
  # train_time_vis = time_sheet %>% ungroup %>% dplyr::select(train_periods) %>%
  #   unnest(train_periods) %>% dplyr::select(train_end) %>% mutate(Value='Timeline') %>%
  #   pivot_longer(!Value, names_to='Type', values_to='Date') %>% mutate(Type='Train_End')
  # test_time_vis = time_sheet %>% ungroup %>% dplyr::select(test_periods) %>%
  #   unnest(test_periods) %>% dplyr::select(test_begin) %>% mutate(Value='Timeline') %>%
  #   pivot_longer(!Value, names_to='Type', values_to='Date') %>% mutate(Type='Test_Begin')
  # time_vis = full_join(train_time_vis, test_time_vis)
  
  train_time_vis = NULL
  test_time_vis = NULL
  for (t in 2:dim(time_sheet)[2]){
    sns = names(time_sheet)
    ts = time_sheet %>% ungroup %>% unnest(sns[t]) 
    
    if ('train_end' %in% colnames(ts)){
      ts = ts %>% dplyr::select(train_end) 
      test_flag = FALSE
      base_n = 'train_end'
    }else{
      ts = ts %>% dplyr::select(test_begin) 
      test_flag = TRUE
      base_n = 'test_begin'
    }
    
    rename = unlist(strsplit(sns[t], '_'))[-(1:2)]
    if (!is_empty(rename)){
      colnames(ts) = paste0(base_n, '_', rename[1], '_', rename[2])
    }
    ts = ts %>% mutate(Value='Timeline') %>% 
      pivot_longer(!Value, names_to='Type', values_to='Date')
    
    if (is.null(train_time_vis)){
      if (!test_flag)
        train_time_vis = ts
    }else{
      if (!test_flag)
        train_time_vis = full_join(train_time_vis, ts)
    }
    
    if (is.null(test_time_vis)){
      if (test_flag)
        test_time_vis = ts
    }else{
      if (test_flag)
        test_time_vis = full_join(test_time_vis, ts)
    }
  }
  
  time_vis = full_join(train_time_vis, test_time_vis)
  
  dates_lim = c(min(time_vis$Date), max(cum_ret$Date))
  tt_plot = time_vis %>%
    ggplot(aes(x=Date, y=Value, color=Type, group=1)) +
    labs(y=' ') +
    geom_point(aes(color=Type, shape=Type), size=2.5, stroke=0.8) +
    theme_tq() +
    scale_color_tq() +
    scale_x_date(limits=dates_lim, breaks=time_sheet$rwt_date, date_labels = "%Y-%m-%d") +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    theme(axis.text.y.left=element_blank()) +
    theme(plot.subtitle = element_text(hjust=0.5)) +
    theme(legend.text = element_text(size=10, face="bold")) +
    theme(axis.text = element_text(size=12)) +
    theme(axis.title = element_text(size=15,face="bold")) +
    theme(plot.title = element_text(size=20,face="bold")) +
    theme(plot.subtitle = element_text(size=12)) +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
    theme(axis.title.x.bottom = element_text(margin = margin(20, 0, 0, 0))) +
    theme(axis.title.y.left = element_text(margin = margin(0, 15, 0, 0))) +
    theme(plot.title = element_text(margin = margin(0, 0, 5, 0))) +
    theme(plot.subtitle = element_text(margin = margin(0, 0, 15, 0))) +
    theme(legend.position='bottom', legend.direction = "horizontal") +
    theme(legend.title = element_text(size=12, face="bold")) +
    theme(legend.text = element_text(size=10, face="bold")) +
    theme(legend.background = element_rect(fill="gray", size=0.5, linetype="solid",  colour ="darkblue")) 
  
  
  pv_plot = cum_ret %>% ggplot(aes(x=Date, y=cumulative_returns, color=Portfolio)) +
    geom_line(size=0.5) +
    labs(title = "Portfolio Evalution Path",
        #  subtitle = paste('Backtest Result From', bt_param$test_start, 'to', max(cum_ret$Date)),
         # caption = "Portfolio 3 is a Standout!",
         x = " ", y = "Portfolio Value",
         color = "Portfolio") +
    coord_trans(y="log2") +
    theme_tq() +
    scale_color_tq() +
    # scale_x_date(breaks=time_sheet$rwt_date, date_labels = "%Y-%m-%d",) +
    # theme(axis.text.x=element_text(angle=60, hjust=1)) +
    # theme(axis.title.x.bottom = element_text(margin = margin(20, 0, 0, 0))) +
    scale_x_date(limits=dates_lim, breaks=time_sheet$rwt_date) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(plot.title = element_text(hjust=0.5)) +
    theme(plot.subtitle = element_text(hjust=0.5)) +
    theme(legend.position='top', legend.direction = "horizontal") +
    theme(legend.title = element_text(size=13, face="bold")) +
    theme(legend.text = element_text(size=10, face="bold")) +
    theme(legend.background = element_rect(fill="gray", size=0.5, linetype="solid",  colour ="darkblue")) +
    theme(axis.text = element_text(size=12)) +
    theme(axis.title = element_text(size=15,face="bold")) +
    theme(plot.title = element_text(size=25,face="bold")) +
    theme(plot.subtitle = element_text(size=12)) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
    theme(axis.title.y.left = element_text(margin = margin(0, 15, 0, 0))) +
    theme(axis.title.y.right = element_text(margin = margin(0, 0, 0, 15))) +
    theme(plot.title = element_text(margin = margin(0, 0, 5, 0))) +
    theme(plot.subtitle = element_text(margin = margin(0, 0, 15, 0))) +
    guides(color = guide_legend(override.aes = list(size = 1.5)))
  
  if (smooth){
    pv_plot = pv_plot + geom_smooth(size=0.2)
  }
  
  
  dd_plot = dd %>% ggplot(aes(x=Date, y=drawdowns, color=factor(Portfolio))) +
    geom_line(size=0.25) +
    labs(x = " ", y = "DrawDowns") +
    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "none") +
    scale_x_date(limits=dates_lim, breaks=time_sheet$rwt_date, date_labels = "%Y-%m-%d",) +
    # theme(axis.text.x=element_text(angle=60, hjust=1)) +
    # theme(axis.title.x.bottom = element_text(margin = margin(20, 0, 0, 0))) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(plot.title = element_text(hjust=0.5)) +
    theme(plot.subtitle = element_text(hjust=0.5)) +
    theme(axis.text = element_text(size=12)) +
    theme(axis.title = element_text(size=13,face="bold")) +
    theme(plot.subtitle = element_text(size=12)) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
    theme(axis.title.y.left = element_text(margin = margin(0, 15, 0, 0))) +
    theme(axis.title.y.right = element_text(margin = margin(0, 0, 0, 15))) +
    # theme(axis.title.x.bottom = element_text(margin = margin(20, 0, 30, 0))) +
    theme(plot.title = element_text(margin = margin(0, 0, 5, 0))) +
    theme(plot.subtitle = element_text(margin = margin(0, 0, 15, 0))) +
    guides(color = guide_legend(override.aes = list(size = 1.5)))

  
  # pv_plot = pv_plot + scale_color_manual(values = c('red', 'black'))
  # dd_plot = dd_plot + scale_color_manual(values = c('red', 'black'))
  
  tb_raster = my_table %>% flextable::flextable() %>% as_raster()
  
  tb_plot = ggplot() + theme_void() +
    annotation_custom(rasterGrob(tb_raster), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  
  # browser()
  port_plots = ggarrange(pv_plot, NULL, 
                         dd_plot, NULL,
                         tt_plot, NULL,
                         tb_plot, NULL,
                         ncol=1, align="v",
                         heights = c(0.3, -0.003, 0.08, -0.002, 0.14, 0.03, 0.2, 0.001))

  # port_plots = ggarrange(pv_plot, NULL, 
  #                        dd_plot, NULL,
  #                        tb_plot, NULL,
  #                        ncol=1, align="v",
  #                        heights = c(0.3, -0.006, 0.08, -0.006, 0.2, 0.001))
  
  wts_smry = port_info %>% 
    dplyr::select(Portfolio, Wts_Summary) %>% 
    filter(Portfolio %in% port_slice) %>% drop_na()
  
  v_agg_plt = NULL
  for (k in 1:dim(wts_smry)[1]){
    unnest_wts_smry = wts_smry[k, ] %>% unnest(Wts_Summary)
    h_agg_plt = NULL
    count = 0
    for (n in names(unnest_wts_smry)[-1]){
      if (grepl('factor', n)){
        main_title = 'Factor Weights Summary'
        color_init = NULL
        linetype_init = NULL
      }else{
        main_title = 'Stock Weights Summary'
        if (grepl('val', n)){
          color_init = c('red', 'blue', 'blue', 'red')
          linetype_init = c('dashed', 'dashed', 'solid', 'solid')
          shape_init = NULL
        }else{
          color_init = c('blue', 'red', 'blue', 'red')
          linetype_init = c('solid', 'solid')
          shape_init = c('triangle', 'triangle')
        }
      }
      
      port_name = unnest_wts_smry$Portfolio%>%unique
      sub_title = paste('Portfolio : ', port_name)
      plt = unnest_wts_smry %>% dplyr::select(c('Portfolio', n)) %>% unnest(n) %>% 
        left_join(daily_cal, ., by='Date') %>% 
        mutate_all(na.locf, na.rm=FALSE, fromLast=FALSE) %>% drop_na() %>%
        pivot_longer(!c(Date, Portfolio), names_to='Type', values_to='val') %>%
        ggplot(aes(x=Date, y=val, color=Type, linetype=Type)) +
        geom_line(size=0.5) +
        labs(title = main_title,
             subtitle = sub_title,
             # caption = "Portfolio 3 is a Standout!",
             x = "Date", y = "Value",
             color = "Type") +
        # theme_tq() +
        # scale_color_tq() +
        # scale_x_date(breaks = function(x) seq.Date(from = min(x), 
        #                                            to = max(x), 
        #                                            by = "1 years"))+
        theme(plot.title = element_text(hjust=0.5)) +
        theme(plot.subtitle = element_text(hjust=0.5)) +
        theme(legend.text = element_text(size=9, face="bold")) +
        theme(legend.position='top', legend.direction = "horizontal") +
        theme(legend.background = element_rect(fill="gray", size=0.5, linetype="solid",  colour ="darkblue")) +
        theme(plot.title = element_text(size=15,face="bold")) +
        theme(axis.title = element_text(size=12,face="bold")) +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        theme(plot.subtitle = element_text(size=12, face="bold")) +
        theme(axis.text = element_text(size=11)) +
        # theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        theme(axis.title.x.bottom = element_text(margin = margin(15, 0, 10, 0))) +
        theme(axis.title.y.left = element_text(margin = margin(0, 5, 0, 0))) +
        theme(plot.title = element_text(margin = margin(0, 0, 5, 0))) +
        theme(plot.subtitle = element_text(margin = margin(0, 0, 15, 0))) + 
        theme(plot.margin = unit(c(2,0,0,0), "cm"))
      
      if (grepl('stk_num', n)){
        plt = plt + scale_y_continuous(labels = scales::number_format(accuracy=1))
      }else{
        plt = plt + scale_y_continuous(labels = scales::number_format(accuracy=0.001))
      }
      
      if (!is.null(color_init) | !is.null(linetype_init)){
        plt = plt + scale_color_manual(values = color_init) +
          scale_linetype_manual(values = linetype_init)
      }
      if (!is.null(shape_init)){
        plt = plt + geom_point(aes(color=Type, shape=Type), size=0.5) +
          scale_shape_manual(values = shape_init)
      }
      
      plt = ggarrange(NULL, plt, NULL, nrow=1, widths = c(0.02, 1, 0.05))
      
      if (is.null(h_agg_plt)){
        h_agg_plt = plt
      }else{
        # h_agg_plt = cowplot::plot_grid(h_agg_plt, plt, nrow=1, ncol=2, rel_widths=c(count, 1))
        h_agg_plt = ggarrange(h_agg_plt, NULL, plt, nrow=1, align="h", widths = c(count, 0.03, 1))
      }
      count = count + 1
    }
    
    if (is.null(v_agg_plt)){
      v_agg_plt = h_agg_plt
    }else{
      v_agg_plt = cowplot::plot_grid(v_agg_plt, h_agg_plt, nrow=2, ncol=1, rel_heights=c(k-1, 1))
      # v_agg_plt = ggarrange(v_agg_plt, h_agg_plt, ncol=1, align="v", widths=c(k-1, 1))
    }
  }
  
  ggsave(save_info$port_v, port_plots, device='pdf', width = 17, height = 15, dpi = 300, units = "in", limitsize = FALSE)
  ggsave(save_info$port_w, v_agg_plt, device='pdf', width = 17, height = 5 + (dim(wts_smry)[1]-1)*20/3, dpi = 300, units = "in", limitsize = FALSE)
  
  qpdf::pdf_combine(input = c(save_info$port_v, save_info$port_w), output = save_info$PDF)
  unlink(save_info$port_v)
  unlink(save_info$port_w)
}
# ======================================================================================================== #




# ======================================================================================================== #
#' @param result_names 
#' @param base_port 
#' @param root_dir 
#' @param target_dir
#' @param base_benchmark
#' 
#' @importFrom zoo coredata
#' @export ins-oos-returns

multi_results <- function(result_names=NULL, base_port=NULL, root_dir=NULL, multi_dir=NULL, base_benchmark=NULL){
  
  # browser(expr=(debug))
  rn = result_names
  
  for (r in seq_along(rn)){
    td = file.path(root_dir, multi_dir, paste0(rn[r], '.RData'))
    load(td)
    
    train_p = paste0('train_periods_', rn[r])
    test_p = paste0('test_periods_', rn[r])
    nm = c('rwt_date', train_p, test_p)
    info_sheet_m <- info_sheet %>% dplyr::select(rwt_date, train_periods, test_periods)
    colnames(info_sheet_m) = nm
    
    agg_sheet = portfolio_info_sheet %>% 
      filter(Portfolio %in% base_port)
    bsp_p <- paste0(agg_sheet$Portfolio , '_', rn[r])
    agg_sheet <- agg_sheet %>% mutate(Portfolio = bsp_p)
    
    if (r == 1){
      info_sheet_multi = info_sheet_m
      agg_sheet_multi = agg_sheet
    }else{
      info_sheet_multi = full_join(info_sheet_multi, info_sheet_m, by='rwt_date')
      agg_sheet_multi = rbind(agg_sheet_multi, agg_sheet)
    }
  }
  
  idx_sheet = portfolio_info_sheet %>% 
    filter(Portfolio %in% base_benchmark) 
  agg_sheet_multi = rbind(agg_sheet_multi, idx_sheet)
  
  return(list(agg=agg_sheet_multi, info=info_sheet_multi))
}
# ======================================================================================================== #
