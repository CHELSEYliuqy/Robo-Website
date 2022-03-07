if (file.exists(save_dir)){
  unlink(save_dir, recursive=TRUE)
}

dir.create(save_dir, recursive=TRUE)

write.csv(portfolio_stat_sheet, file=file.path(save_dir, 'stat.csv'), row.names = F)


target_wts_list <- portfolio_info_sheet %>% dplyr::select(Portfolio, Wts_List) %>% filter(Portfolio %in% base_port)
for (p in target_wts_list$Portfolio){
  port_wts_list <- target_wts_list %>% filter(Portfolio == p) %>% .$Wts_List %>% .[[1]] 
  port_wts_list <- port_wts_list %>% mutate_if(is.factor, factor_to_double)
  latest_wts <- tail(port_wts_list, 1) %>% dplyr::select(-Date) %>% as.data.frame() 
  latest_nonzero_wts <- latest_wts[latest_wts!=0]
  latest_nonzero_ticks <- names(latest_wts)[latest_wts!=0]
  latest_nonzero_ticks = c(latest_nonzero_ticks, 'Cash')
  latest_nonzero_wts = c(latest_nonzero_wts, 1-sum(latest_nonzero_wts %>% as.numeric))
  latest_info <- tibble(Code = latest_nonzero_ticks, Weigths = latest_nonzero_wts)
  write.csv(latest_info, file=file.path(save_dir, paste0(p, '_latest_wts.csv')), row.names=F)
}


target_wts_summary = portfolio_info_sheet %>% dplyr::select(Portfolio, Wts_Summary) %>% filter(Portfolio %in% base_port)
for (p in target_wts_list$Portfolio){
  port_wts_summary <- target_wts_summary %>% unnest(Wts_Summary) %>% 
    unnest %>% dplyr::select(-Date1, -Date2) %>% filter(Portfolio == p)
  write.csv(port_wts_summary, file.path(save_dir, paste0(p, '_wts_summary.csv')),  row.names=F)
}


cumret = portfolio_info_sheet %>% dplyr::select(Portfolio, Cum_Return_Cost) %>% unnest(Cum_Return_Cost) %>% 
  filter(Portfolio %in% selected_port) %>% pivot_wider(names_from = Portfolio, values_from = cumulative_returns) 
cumret = cumret[, !apply(is.na(cumret), 2, any)] 
cumret <- cumret %>% mutate_if(is.numeric, function(x) x * as.numeric(init_cap))
write.csv(cumret, file=file.path(save_dir, 'cumret_all.csv'),  row.names=F)



drawdown <- portfolio_info_sheet %>% dplyr::select(Portfolio, DrawDown) %>% unnest(DrawDown) %>% 
  filter(Portfolio %in% selected_port) %>% pivot_wider(names_from = Portfolio, values_from = drawdowns) 
drawdown = drawdown[, !apply(is.na(drawdown), 2, any)]
write.csv(drawdown, file=file.path(save_dir, 'drawdown_all.csv'),  row.names=F)
