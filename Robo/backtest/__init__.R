# load dataset
load(file.path(data_dir, 'DATASET.RData'))

daily_calendar <- calendar_dataset$calendar$daily

# choose calendar or volume clock dataset
if (btp$clock == 'calendar'){
  dataset <- calendar_dataset
  if (!is.na(btp$sigma_annual)){
    if (btp$calc_param$train_freq == 'daily'){
      btp$sigma <- btp$sigma_annual / sqrt(252)
    }else if (btp$calc_param$train_freq == 'weekly'){
      btp$sigma <- btp$sigma_annual / sqrt(52)
    }else if (btp$calc_param$train_freq == 'monthly')
      btp$sigma <- btp$sigma_annual / sqrt(12)
  }
  info_sheet <- calendar_periods_segment(calendar=dataset$calendar, bt_param=btp, adj_margin=10)
  
  # specify training and testing data
  stk_train <- dataset$stock %>% .[[btp$market]] %>% 
    .[[btp$calc_param$train_freq]] %>% mutate(type = 'stock') %>% arrange(Date)
  stk_train_pivot <- stk_train %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>% arrange(Date)
  stk_test <- dataset$stock %>% .[[btp$market]] %>% 
    .[[btp$calc_param$eval_freq]] %>% mutate(type = 'stock') %>% arrange(Date)
  stk_test_pivot <- stk_test %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>% arrange(Date)
  
  fct_train <- dataset %>% .[[btp$factor_used]] %>% .[[btp$calc_param$train_freq]] %>%
    filter(Code %in% (btp$factor_name %>% .[[1]])) %>%
    mutate(type = 'factor') %>% arrange(Date) %>%
    mutate_at('rf_return', replace_na, 0)
  fct_train_pivot <- fct_train %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>% 
    mutate_if(is.numeric, as.factor) %>% arrange(Date)
  fct_test <- dataset %>% .[[btp$factor_used]]%>% .[[btp$calc_param$eval_freq]] %>%
    filter(Code %in% (btp$factor_name %>% .[[1]])) %>%
    mutate(type = 'factor') %>% arrange(Date)
  fct_test_pivot <- fct_test %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>%
    mutate_if(is.numeric, as.factor) %>% arrange(Date)
  
  train_pivot <- full_join(stk_train_pivot, fct_train_pivot, by='Date') %>% arrange(Date)
  test_pivot <- full_join(stk_test_pivot, fct_test_pivot, by="Date") %>% arrange(Date)
  
  rf_test <- dataset %>% .$risk_free %>%.[[btp$calc_param$eval_freq]]
  
  idx_test <- dataset$INDEX  %>% .[[btp$calc_param$eval_freq]] %>%
    dplyr::select(Date, Code, rf_return) %>% 
    pivot_wider(names_from=Code, values_from=rf_return)
  
  etf_test <- dataset$ETF %>% .[[btp$calc_param$eval_freq]] %>%
    dplyr::select(Date, Code, rf_return) %>% 
    pivot_wider(names_from=Code, values_from=rf_return)
  
  index_info <- dataset$index_history %>% .[[btp$market]]
  
}else{
  
  dataset <- volume_dataset
  btp$sigma <- dataset$INDEX %>% 
    filter(Code == btp$market) %>% 
    .[['rf_return']] %>% sd(., na.rm=TRUE) / 2
  
  info_sheet <- volume_periods_segment(calendar=dataset$calendar, bt_param=btp)
  
  stk_train <- dataset$stock %>% .[[btp$market]] %>% 
    mutate(type = 'stock') %>% arrange(Date)
  stk_train_pivot <- stk_train %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>% arrange(Date)
  stk_test <- calendar_dataset$stock %>% .[[btp$market]] %>% .$daily %>%
    mutate(type = 'stock') %>% arrange(Date)
  stk_test_pivot <- stk_test %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>% arrange(Date)
  
  fct_train <- dataset %>% .[[btp$factor_used]] %>% 
    filter(Code == btp$factor_name %>% .[[1]]) %>%
    mutate(type = 'factor') %>% arrange(Date)
  fct_train_pivot <- fct_train %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>% 
    mutate_if(is.numeric, as.factor) %>% arrange(Date)
  fct_test <- calendar_dataset %>% .[[btp$factor_used]]%>% .$daily %>%
    filter(Code %in% (btp$factor_name %>% .[[1]])) %>%
    mutate(type = 'factor') %>% arrange(Date)
  fct_test_pivot <- fct_test %>% dplyr::select(Date, Code, rf_return) %>%
    pivot_wider(names_from=Code, values_from=rf_return) %>%
    mutate_if(is.numeric, as.factor) %>% arrange(Date)
  
  train_pivot <- full_join(stk_train_pivot, fct_train_pivot, by='Date') %>% arrange(Date)
  test_pivot <- full_join(stk_test_pivot, fct_test_pivot, by="Date") %>% arrange(Date)
  
  rf_test <- calendar_dataset$risk_free$daily
  
  idx_test <- calendar_dataset$INDEX$daily %>% 
    dplyr::select(Date, Code, rf_return) %>% 
    pivot_wider(names_from=Code, values_from=rf_return)
  
  etf_test <- calendar_dataset$ETF$daily %>%
    dplyr::select(Date, Code, rf_return) %>% 
    pivot_wider(names_from=Code, values_from=rf_return)
  
  index_info <- calendar_dataset$index_history %>% .[[btp$market]]
}


# random seed
random_seed <- seq(from=100, by=1, length.out=dim(info_sheet)[1])


# multicore settings for future_map
rwt_num <- dim(info_sheet)[1]
avlb_cores <- availableCores()
if (!is.na(btp$multi_core)){
  if (btp$multi_core > avlb_cores) btp$multi_core <- avlb_cores
  future::plan(multicore, workers=btp$multi_core)
}else{
  future::plan(sequential)
}
options(future.globals.maxSize = 1048576000) # 1000MB
cat('\n')
cat('\n')
cat(avlb_cores, 'Cores Available', '\n')
cat(btp$multi_core, 'Cores Assign to Processing Data', '\n')
cat('\n')