
cat('Calulate', btp$reb_freq, 'Rebalance Weights', '\n')
with_progress({
  pb <- progressor(steps=dim(portfolio_wts_sheet)[1])
  portfolio_wts_sheet <- portfolio_wts_sheet %>%
    mutate(Reb_Wts_List = future_map(Wts_List, ~rebalance_weights(.x,
    daily_cal=daily_calendar, reb_freq=btp$reb_freq, pb=pb), 
   .options=furrr_options(seed=NULL)))
})
cat('Done', '\n')
cat('\n')




# ======================================================================================================== #
# Weights Summary
# ======================================================================================================== #
cat('Summarize Weights', '\n')
with_progress({
  pb <- progressor(steps=dim(portfolio_wts_sheet)[1])
  portfolio_wts_sheet <- portfolio_wts_sheet %>%
    mutate(Wts_Summary = future_map(Wts_List, ~wts_summary(.x, pb=pb), 
    .options=furrr_options(seed=NULL)))
})
cat('Done', '\n')
cat('\n')
# ======================================================================================================== #




# ======================================================================================================== #
# Portfolio Returns, BOP Weights, EOP Weights, Transaction Cost, etc
# ======================================================================================================== #
cat('Calulate Portfolio Returns, BOP Weights, EOP Weights, Transaction Cost, etc', '\n')
options(warn=-1)
with_progress({
  pb <- progressor(steps=dim(portfolio_wts_sheet)[1])
  portfolio_info_sheet <- portfolio_wts_sheet %>%
    mutate(port_info = future_map(Reb_Wts_List, ~portfolio_info(.x, asset_pivot=test_pivot, 
    bt_param=btp, pb=pb), .options=furrr_options(seed=NULL))) %>% unnest(port_info)
})
options(warn=0)
cat('Done', '\n')
cat('\n')
# ======================================================================================================== #




# ======================================================================================================== #
# Aggregate Benchmarks Returns
# ======================================================================================================== #
exist_ret <- full_join(etf_test, idx_test, by='Date') %>% arrange(Date) %>%
  as_tbl_time(Date) %>% filter_time(portfolio_info_sheet$Rf_Return[[1]]$Date[1] ~ btp$test_end) %>%
  pivot_longer(!Date, names_to='Portfolio', values_to='portfolio.returns') %>% 
  group_by(Portfolio) %>% nest() %>% rename(Rf_Return=data) %>% ungroup %>%
  mutate(Rf_Return_Cost = Rf_Return)

benchmark <- exist_ret %>% filter(Portfolio %in% btp$market) %>%
    dplyr::select(Rf_Return) %>% .[[1]]

portfolio_info_sheet <- portfolio_info_sheet %>% 
  full_join(., exist_ret, by=c("Portfolio", "Rf_Return", "Rf_Return_Cost")) %>%
  mutate(Benchmark_Rf_Return = benchmark) %>%
  relocate(Benchmark_Rf_Return, .before = Rf_Return)
# ======================================================================================================== #




# ======================================================================================================== #
# Calculate Raw Returns, Cumulative Returns, DrawDowns, etc
# ======================================================================================================== #
cat('Calculate Raw Returns, Cumulative Returns, DrawDowns, etc', '\n')
with_progress({
  pb <- progressor(steps=dim(portfolio_wts_sheet)[1])
  portfolio_info_sheet <- portfolio_info_sheet %>%
    mutate(Raw_Return = future_map(Rf_Return, ~risk_free_adjust(.x, 
      risk_free=rf_test, first_rwt_date=info_sheet$rwt_date[1], 
      pb=pb), .options=furrr_options(seed=NULL))) %>%
    relocate(Raw_Return, .after = Rf_Return) %>%
    mutate(Raw_Return_Cost = future_map(Rf_Return_Cost, ~risk_free_adjust(.x, 
      risk_free=rf_test, first_rwt_date=info_sheet$rwt_date[1], 
      pb=pb), .options=furrr_options(seed=NULL))) %>%
    relocate(Raw_Return_Cost, .after = Rf_Return_Cost) %>%
    mutate(Cum_Return_Cost = future_map(Raw_Return_Cost, ~cumulative_return(.x), 
    .options=furrr_options(seed=NULL))) %>%
    relocate(Cum_Return_Cost, .after=Raw_Return_Cost) %>%
    mutate(DrawDown = future_map(Raw_Return_Cost, ~drawdown(.x), 
    .options=furrr_options(seed=NULL)))
})
cat('Done', '\n')
cat('\n')
# ======================================================================================================== #
  

  

# ======================================================================================================== #
# Calculate Portfolio Statistics
# ======================================================================================================== #
cat('Calculate Portfolio Statistics', '\n')
with_progress({
  pb <- progressor(steps=dim(portfolio_wts_sheet)[1])
  portfolio_stat_sheet <- portfolio_info_sheet %>% filter(Portfolio %in% selected_port) %>%
    mutate(Statistics=future_pmap(list(Rf_Return_Cost, 
      Raw_Return_Cost, Benchmark_Rf_Return), agg_statistics, 
      bt_param=btp, pb=pb, .options=furrr_options(seed=NULL))) %>%
    dplyr::select(Portfolio, Statistics) %>% 
    unnest(Statistics) %>% print()
})
cat('Done', '\n')
cat('\n')
# ======================================================================================================== #

