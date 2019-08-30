source("install_required_packages.r")
source("synth_validation_benchmark.r")

library(tictoc)

tic("total")
for(i in 1:15){
  tic(paste("real_data_raw/data_1000_1_run",i,sep = ""))
  benchmark("real_data_raw/data_1000_1",200,100,FALSE,TRUE)
  toc()
}
toc()

