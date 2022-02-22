args <- commandArgs(trailingOnly = TRUE)

if (args[3] == 'SSE50'){
    a = data.frame(matrix(rep(1, 100), 10, 10))
    write.csv(a, file = '/Users/ruizhaohuang/Desktop/Robo/Robo/bt_outcome/123.csv')
}