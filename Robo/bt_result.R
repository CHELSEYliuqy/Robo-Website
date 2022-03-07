args <- commandArgs(trailingOnly = TRUE)


# args[1] --> cap
# args[2] --> sigma
# args[3] --> market
# args[4] --> rebalance frequency
# args[5] --> test start date
# args[6] --> test end date
# args[7] to args[n] --> selected portfolio

if (args[7] == 'MAXSER'){
    a = data.frame(matrix(rep(1, 100), 10, 10))
    write.csv(a, file = '/Users/ruizhaohuang/Documents/Github/Robo-Website/Robo/bt_outcome/123.csv')
}

init_cap <- args[1]
sigma <- args[2]
market <- args[3]
reb_freq <- args[4]
test_start <- args[5]
test_end <- args[6]
selected_port <- NULL
for (s in 7:length(args)){
    selected_port <- c(selected_port, args[s])
}


# root_dir <- '/Users/ruizhaohuang'
# base_result_dir <- '/Documents/Github/MAXSER-BACKTEST-V3/Backtest_Results_v3'
# load(file.path(root_dir, base_result_dir, market, 'LATEST/latest.RData'))