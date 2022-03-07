
multi_res <- multi_results(result_names=multi_names, base_port=base_port,
                           root_dir=save_root_dir, multi_dir=multi_dir,
                           base_benchmark=base_benchmark)

# aa <- calendar_dataset$ETF_Factor_Long$daily %>% mutate(Code='ETF_Factor_Long_5year') %>%
#     as_tbl_time(Date) %>% 
#     filter_time(btp$test_start ~ btp$test_end) %>% as_tibble %>%
#     rename(Portfolio=Code, portfolio.returns=rf_return) %>% 
#     group_by(Portfolio) %>% nest() %>% rename(Rf_Return=data) %>% ungroup %>%
#     mutate(Rf_Return_Cost = Rf_Return)
# aa <-  aa %>%
#     mutate(Raw_Return = future_map(Rf_Return, ~risk_free_adjust(.x, 
#       risk_free=rf_test, first_rwt_date=info_sheet$rwt_date[1], 
#       pb=NULL), .options=furrr_options(seed=NULL))) %>%
#     relocate(Raw_Return, .after = Rf_Return) %>%
#     mutate(Raw_Return_Cost = future_map(Rf_Return_Cost, ~risk_free_adjust(.x, 
#       risk_free=rf_test, first_rwt_date=info_sheet$rwt_date[1], 
#       pb=NULL), .options=furrr_options(seed=NULL))) %>%
#     relocate(Raw_Return_Cost, .after = Rf_Return_Cost) %>%
#     mutate(Cum_Return_Cost = future_map(Raw_Return_Cost, ~cumulative_return(.x), 
#     .options=furrr_options(seed=NULL))) %>%
#     relocate(Cum_Return_Cost, .after=Raw_Return_Cost) %>%
#     mutate(DrawDown = future_map(Raw_Return_Cost, ~drawdown(.x), 
#     .options=furrr_options(seed=NULL)))

# benchmark <- multi_res$agg %>% filter(Portfolio %in% btp$market) %>%
#     dplyr::select(Rf_Return) %>% .[[1]]

# aa = aa %>% mutate(Benchmark_Rf_Return = benchmark)

# multi_res$agg <- full_join(multi_res$agg, aa)


multi_stat <- multi_res$agg %>% group_by(Portfolio) %>%
    transmute(Statistics = pmap(list(Rf_Return_Cost, 
    Raw_Return_Cost, Benchmark_Rf_Return), agg_statistics, 
    bt_param=btp)) %>% unnest(Statistics) %>% print()

port_slice <- c(paste0(base_port, '_', multi_names), base_benchmark)
# port_slice <- c('CSI500', 'MAXSER_ALL_Long_Only_CSI500ETF-3years')
# port_slice <- c('HS300', 'MAXSER_ALL_Long_Only_HS300ETF-3years')
portfolio_value_plot(port_info=multi_res$agg,
                     port_stat=multi_stat,
                     save_info=save_files,
                     smooth=FALSE,
                     port_slice=port_slice,
                     bt_param=btp,
                     time_sheet=multi_res$info,
                     daily_cal=daily_calendar)


wts_list <-  multi_res$agg[1, ]$Wts_List[[1]]
latest_wts <- wts_list[dim(wts_list)[1], ] %>% as.data.frame() %>% .[-1]
non_zero_wts <- latest_wts[which(latest_wts!=0)]
latest_tickers <- names(non_zero_wts)
non_zero_wts <- tibble(Code=latest_tickers, wts=as.numeric(non_zero_wts))


stk_data <- calendar_dataset$stock[[btp$market]]$daily 
latest_dates <- stk_data$Date %>% max()
latest_close <- stk_data %>% filter(Code %in% latest_tickers) %>% 
  filter(Date == latest_dates) %>% dplyr::select(Date, Code, close)

latest_info <- left_join(latest_close, non_zero_wts, by='Code')
latest_info <- latest_info %>% mutate(cap = 1e+6) %>%
  mutate(opt_amount = cap * wts) %>%
  mutate(opt_shares = opt_amount / close) %>%
  mutate(act_shares = round(opt_shares / 100) * 100) %>%
  mutate(act_amount = act_shares * close) %>%
  relocate(act_amount, .after=opt_amount)


