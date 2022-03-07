args <- commandArgs(trailingOnly = TRUE)

init_cap <- args[1]
sigma_target <- args[2]
market <- args[3]
reb_freq <- args[4]
test_start <- args[5]
test_end <- args[6]
selected_port <- NULL
for (s in 7:length(args)){
    selected_port <- c(selected_port, args[s])
}

# init_cap <- '1000000'
# sigma_target <- '0.2'
# market <- 'SSE50'
# reb_freq <- 'weekly'
# test_start <- '2017-01-01'
# test_end <- '2021-12-01'
# selected_port <- c('EW', 'MAXSER', 'MAXSER_ALL_Long_Only')


root_dir <- '/Users/ruizhaohuang'
work_dir <- 'Documents/Github/Robo-Website/Robo/backtest'
base_result_dir <- '/Documents/Github/MAXSER-BACKTEST-V3/Backtest_Results_v3'
save_dir <- file.path(root_dir, 'Documents/Github/Robo-Website/Robo/bt_outcome')
base_sigma <- 0.18
base_port <- c('EW', 'Factor', 'Risk_Parity', 'Min_Var', 'MAXSER', 'MAXSER_H',
             'MAXSER_Stock_Long_Only', 'MAXSER_ALL_Long_Only', 'MAXSER_H_Stock_Long_Only', 'MAXSER_H_ALL_Long_Only')
risk_non_port <- c('EW', 'Risk_Parity', 'Min_Var')
risk_parm_port <- setdiff(base_port, risk_non_port)


setwd(file.path(root_dir, work_dir))
source('import.R')
source('utils.R')


load(file.path(root_dir, base_result_dir, market, 'LATEST/latest.RData'))


if (market == 'HSI'){
  data_dir <- file.path(root_dir, 'Documents/Data/HK-Data-v3/Rdata')
  benchmark_name <- 'HSI'
}else if (market == 'SP500'){
  data_dir <- file.path(root_dir, 'Documents/Data/US-Data/Rdata')
  benchmark_name <- 'SP500'
}else{
  data_dir <- file.path(root_dir, 'Documents/Data/A-shares-v4/Rdata')
  benchmark_name <- c('SSE50', 'SSE', 'HS300', 'CSI500', 'SZ100', 'CYB50')
}

selected_port <- c(benchmark_name, selected_port)
console_file <- paste0(market, '-console.R')

source(console_file)
source('__init__.R')


wts_adjust <- function(wts, adj){
  wts_adj <- wts %>% mutate_if(is.numeric, function(x) x * adj)
  return(wts_adj)
}

risk_non_wts_sheet <- portfolio_wts_sheet %>% filter(Portfolio %in% risk_non_port)
portfolio_parm_wts_sheet <- portfolio_wts_sheet %>% filter(Portfolio %in% risk_parm_port) %>%
  mutate(Wts_List = map(Wts_List, ~wts_adjust(.x, adj=as.numeric(sigma_target)/base_sigma)))
portfolio_wts_sheet <- rbind(risk_non_wts_sheet, portfolio_parm_wts_sheet)

with_progress({
  pb <- progressor(steps=dim(portfolio_wts_sheet)[1])
  portfolio_wts_sheet <- portfolio_wts_sheet %>%
    mutate(Reb_Wts_List = future_map(Wts_List, ~rebalance_weights(.x,
    daily_cal=daily_calendar, reb_freq=btp$reb_freq, pb=pb), 
   .options=furrr_options(seed=NULL)))
})

source('backtest.R')
source('save.R')

