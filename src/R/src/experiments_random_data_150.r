source("install_required_packages.r")
source("synth_validation_benchmark.r")

library(tictoc)

tic("total")
for(i in 1:15){
  tic(paste("random_generated_data_",i,sep = ""))
  benchmark(NULL,100,150,FALSE,TRUE)
  toc()
}
toc()

