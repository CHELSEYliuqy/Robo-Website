library(xts)
library(lubridate)
library(PerformanceAnalytics)


market <- 'HS300'
etf_name <- 'HS300_ETF'


start <- '2009-12-31'
end <- NULL


stock_ret <- calendar_dataset$stock[[market]]$daily %>% 
  dplyr::select(Date, Code, rf_return) %>% 
  pivot_wider(names_from=Code, values_from=rf_return) %>%
  tbl_xts() %>% .[paste0(start, '::')]


idx_ret <- calendar_dataset$INDEX$daily %>% 
  filter(Code == market) %>% dplyr::select(Date, Code, rf_return) %>% 
  pivot_wider(names_from=Code, values_from=rf_return) %>%  
  tbl_xts() %>% .[paste0(start, '::', end)] %>% .[-1]


etf_ret <- calendar_dataset$ETF$daily %>%
  filter(Code == etf_name) %>% dplyr::select(Date, Code, rf_return) %>% 
  pivot_wider(names_from=Code, values_from=rf_return) %>%  
  tbl_xts() %>% .[paste0(start, '::', end)] %>% .[-1]


idx_history <- calendar_dataset$index_history[[market]]
months_dates <- index(stock_ret[endpoints(stock_ret, on="months", k=1), ])
quarter_dates <- index(stock_ret[endpoints(stock_ret, on="quarters", k=1), ])
rwt_dates <- quarter_dates


data_period <- list()
for (i in seq_along(rwt_dates)){
  if (i != 1){
    data_period[[i-1]] <- paste0(rwt_dates[i-1], '::', rwt_dates[i])
  }
}


port_ret <- NULL
eigen_wts <- list()
stock_pool <- list()
dates <- list()


for (i in seq_along(data_period)){
  pool <- idx_history %>% filter(Date == rwt_dates[i] & is_in == 1) %>% .$Code
  data <- stock_ret[data_period[[i]]][, pool]
  data <- data[-1]
  data <- na.fill(data, fill=0)
  # data[coredata(abs(data) > 1)] <- 0
  
  T <- dim(data)[1]
  N <- dim(data)[2]
  
  eig <- eigen(crossprod(data)/T)
  v_hat <- eig$vectors[,1]
  v_hat <- v_hat / sum(v_hat)
  
  eigen_wts[[i]] <- v_hat
  stock_pool[[i]] <- pool
  dates[[i]] <- rwt_dates[i]
  
  port <- Return.portfolio(R=data, weights=v_hat, contribution=TRUE, verbose=TRUE)
  if(is.null(port_ret)){
    port_ret <- port$returns
  }else   
    port_ret <- rbind(port_ret, port$returns)
}


ret <- cbind(idx_ret, etf_ret, port_ret)
colnames(ret) <- c(market, etf_name, 'Eigen-Portfolio')
charts.PerformanceSummary(ret)


ew <- list()
for (i in seq_along(eigen_wts)){
  ew[[i]] <- rep(1/300, 300)
}

dis <- c()
for (i in seq_along(eigen_wts)){
  dis[i] <- sum(abs(eigen_wts[[i]] - ew[[i]]))
}


