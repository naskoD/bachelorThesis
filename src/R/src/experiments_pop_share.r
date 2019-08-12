source("install_required_packages.r")
source("synth_validation_benchmark.r")

library(tictoc)


tic("real_data_raw/data_1000_1")
benchmark("real_data_raw/data_1000_1",100,150,FALSE,TRUE)
toc()

tic("real_data_raw/data_1000_2")
benchmark("real_data_raw/data_1000_2",100,150,FALSE,TRUE)
toc()

tic("real_data_raw/data_1000_3")
benchmark("real_data_raw/data_1000_3",100,150,FALSE,TRUE)
toc()

tic("real_data_raw/data_25000_1")
benchmark("real_data_raw/data_25000_1",100,150,FALSE,TRUE)
toc()

tic("real_data_raw/data_25000_2")
benchmark("real_data_raw/data_25000_2",100,150,FALSE,TRUE)
toc()

tic("real_data_raw/data_50000_1")
benchmark("real_data_raw/data_50000_1",100,150,FALSE,TRUE)
toc()

tic("real_data_raw/data_50000_2")
benchmark("real_data_raw/data_50000_2",100,150,FALSE,TRUE)
toc()

tic("real_data_simulated/data_hd_1")
benchmark("real_data_simulated/data_hd_1",100,150,FALSE,TRUE)
toc()

tic("real_data_simulated/data_hd_2")
benchmark("real_data_simulated/data_hd_2",100,150,FALSE,TRUE)
toc()

tic("real_data_simulated/data_ld_1")
benchmark("real_data_simulated/data_ld_1",100,150,FALSE,TRUE)
toc()

tic("random_generated_data")
benchmark(NULL,100,150,FALSE,TRUE)
toc()
