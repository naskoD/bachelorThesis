library(tictoc)

source("synth_validation_benchmark.r")

tic("total")
benchmark("real_data_raw/data_1000_1",100,3,TRUE,TRUE)
toc()

tic("total")
benchmark("real_data_raw/data_1000_1",100,3,FALSE,TRUE)
toc()

tic("total")
generate_benchmark_data_plots(all = TRUE)
toc()

