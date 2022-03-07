#' --------------------------------Parameters List---------------------------------
#' @param market select the backtesting market, eg. SSE50, HS300, CSI500, HSI, SP500, etc
#' @param clock calendar or volume clock
#' @param test_start 
#' @param test_end 
#' @param train_freq 
#' @param eval_freq 
#' @param eval_freq_dates
#' @param reb_freq 
#' @param rwt_freq 
#' @param train_wd 
#' @param train_begin_lag 
#' @param train_end_lag 
#' @param factor_used 
#' @param factor_name 
#' @param sigma 
#' @param sigma_annual 
#' @param reb.freq 
#' @param rwt.freq 
#' @param train.wd 
#' @param sigma 
#' @param sigma.annual
#' @param stock_screen 
#' @param trans_cost 
#' @param fac_wts_constraint
#' @param cv_risk_adj 
#' @param cv_lev_adj 
#' @param est_method
#' @param picks_num 
#' @param picks_id 
#' @param decrease 
#' @param pure_random
#' @param theta_loc
#' @param rm_outlier
#' @param rm_alpha 
#' @param plot_smooth
#' @param EW
#' @param Factor
#' @param MAXSER
#' @param MAXSER_FC 
#' @param MAXSER_SLO 
#' @param MAXSER_ALO 
#' @param MAXSER_H 
#' @param multi_core 
#' @param MAXSER_FC 


btp <- tibble(

  market = 'CYB50',

  clock = 'calendar',

  test_start = test_start,
  test_end = test_end,
  reb_freq = reb_freq,

  calc_param = tibble(
    train_freq = 'daily',
    eval_freq = 'daily',
    eval_freq_dates = 'daily',
    rwt_freq = '6 months',
    train_wd = '5 years',
    train_begin_lag = NA,
    train_end_lag = NA,
  ),

  volc_param = tibble(
    rwt_freq = 25,
    train_wd = 300,
  ),

  factor_used = 'ETF',
  factor_name = tibble('CYB_ETF'),

  sigma = NA,
  sigma_annual = as.numeric(sigma_target),

  stk_screen_mtd = NA,
  stk_pool_num = NA,
  stk_pool = NA,
  pure_random = FALSE,
  theta_loc = 950,

  trans_cost = tibble(stock=0.002, factor=0.001),

  fac_wts_constraint = -0.6,
  cv_risk_adj = TRUE,
  cv_lev_adj = FALSE,
  est_method = 'theta-all',

  rm_outlier = FALSE,
  rm_alpha = 0.05,

  plot_smooth = FALSE,

  EW = TRUE,
  Factor = TRUE,
  MAXSER = TRUE,
  MAXSER_H = TRUE,
  MAXSER_FC = TRUE,     # factor constraint
  MAXSER_SLO = TRUE,    # stock long only
  MAXSER_ALO = TRUE,    # all long only
  MAXSER_H_SLO = TRUE,  # stock long only
  MAXSER_H_ALO = TRUE,  # all long only

  multi_core = 60,
)
