library(tictoc)

source("synth_validation_benchmark.r")


#benchmark()

tic("total")
benchmark("real_data_raw/data_50000_1",100,100,FALSE,FALSE)
toc()