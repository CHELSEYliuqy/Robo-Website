#------------------------------------------------------------------------------------------#
#-----------------------------------------Packages-----------------------------------------#
#------------------------------------------------------------------------------------------#
require(tidyverse)
require(tibbletime)
require(data.table)
require(tidyquant)
require(xts)
require(tbl2xts)
require(lubridate)
require(PerformanceAnalytics)
require(doParallel)
require(foreach)
require(doSNOW)
require(openxlsx)
require(PACLasso)
require(lars)
require(nlshrink)
require(webshot)
require(ggplot2)
require(grid)
require(scales)
require(furrr)
require(future)
require(ggpubr)
require(progressr)
require(RhpcBLASctl)
require(tictoc)
require(lpSolve)
require(Rlab)
require(quadprog)
require(MASS)
require(FarmTest)
require(stats)
require(qpdf)
require(magick)
require(flextable)



#------------------------------------------------------------------------------------------#
#---------------------------------------Source Code----------------------------------------#
#------------------------------------------------------------------------------------------#
source("./source/SR.est.R")
source("./source/MAXSER_weight.R")
source("./source/MAXSER_weight_long_only.R")
source("./source/MAXSER_weight_CV_Constraint_v2.R")
source('./source/MAXSER_weight_factor_constraint_v3.R')
source("./source/MAXSER_HTV.R")
source("./source/MAXSER_HTV_long_only.R")
source("./source/MAXSER_HTV_factor_constraint.R")
source('portfolio_library.R')

Env <- ls(.GlobalEnv)
