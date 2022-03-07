library(PerformanceAnalytics)
dataset1_dir <- '../../../Data/HK-Data-v3/Rdata'
dataset2_dir <- '../../../Data/A-shares-v4/Rdata'

result_root_dir <- file.path('../Backtest_Results_v3', 'Integrate')
result1_name <- '2800-10years-monthly.RData'
result2_name <- 'SSEETF-3years.RData'

load(file.path(dataset1_dir, 'DATASET.RData'))
dataset1 <- calendar_dataset
load(file.path(dataset2_dir, 'DATASET.RData'))
dataset2 <- calendar_dataset
rm(calendar_dataset, volume_dataset)

load(file.path(result_root_dir, result1_name))
wts1 <- portfolio_wts_sheet %>% 
  filter(Portfolio == 'MAXSER_Fac_Constraint') %>% .$Wts_List %>% .[[1]]
ret1 <- portfolio_info_sheet %>% 
  filter(Portfolio == 'MAXSER_Fac_Constraint') %>% 
  .$Raw_Return_Cost %>% .[[1]] %>% rename(MAXSER_HSI = portfolio.returns)
idx1 <- portfolio_info_sheet %>% 
  filter(Portfolio == 'HSI') %>% 
  .$Raw_Return_Cost %>% .[[1]] %>% rename(HSI = portfolio.returns)

load(file.path(result_root_dir, result2_name))
wts2 <- portfolio_wts_sheet %>% 
  filter(Portfolio == 'MAXSER_ALL_Long_Only') %>% .$Wts_List %>% .[[1]]
ret2 <- portfolio_info_sheet %>% 
  filter(Portfolio == 'MAXSER_ALL_Long_Only') %>% 
  .$Raw_Return_Cost %>% .[[1]] %>% rename(MAXSER_SSE50 = portfolio.returns)
idx2 <- portfolio_info_sheet %>% 
  filter(Portfolio == 'SSE50') %>% 
  .$Raw_Return_Cost %>% .[[1]] %>% rename(SSE50 = portfolio.returns)

a <- full_join(ret1, ret2, by='Date') %>% 
  arrange(Date) %>% filter(Date <= '2021-11-29') %>%
  mutate_if(is.numeric, replace_na, 0) %>% tbl_xts

b <- full_join(idx1, idx2, by='Date') %>%
  arrange(Date) %>% filter(Date <= '2021-11-29') %>%
  mutate_if(is.numeric, replace_na, 0) %>% tbl_xts

wts <- c(1/2, 1/2)
ret_all <- Return.portfolio(R=a, weights=wts, contribution=TRUE, rebalance_on='months', verbose=FALSE)$portfolio.returns

sd(ret_all) * sqrt(252)
mean(ret_all) * 252 / sd(ret_all) / sqrt(252)

charts.PerformanceSummary(R=cbind(ret_all, a, b), wealth.index=TRUE)
    


