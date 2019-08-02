library(tictoc)

setwd("thesis/src/R/src/")
source("synth_validation_benchmark.r")

tic("total")
generate_benchmark_data_plots("real_data_raw/data_50000_1",100,100,FALSE,FALSE)
toc()

tic("total")
generate_benchmark_data_plots(all = TRUE)
toc()

#benchmark()

tic("total")
benchmark("real_data_raw/data_1000_1",100,3,TRUE,TRUE)
toc()


