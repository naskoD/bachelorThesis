## Install required packages =============================================================

#install cran packages
packages = c("tictoc","assertthat","psych","collections","MASS","NlcOptim","mltools","data.table","devtools","grf","gridExtra")
packagecheck <- match( packages, utils::installed.packages()[,1] )

packagestoinstall <- packages[ is.na( packagecheck ) ]

if( length( packagestoinstall ) > 0L ) {
  utils::install.packages( packagestoinstall,
                           repos = "http://cran.us.r-project.org"
  )
} else {
  print( "All required cran packages already installed" )
}
#finish installing cran packages

#install packages from github
packages_from_github_names<-c("rlearner")
packages_from_github_repos<-c("xnie/rlearner")

packagecheck <- match( packages_from_github_names, utils::installed.packages()[,1] )

packagestoinstall <- packages_from_github_repos[ is.na( packagecheck ) ]

if( length( packagestoinstall ) > 0L ) {
  library(devtools)
  
  for(p in packagestoinstall){
    install_github(p)
  }
  
} else {
  print( "All required github packages already installed" )
}
#finish installing packages from github

## Finish installing required packages =======================================================

library(tictoc)

source("synth_validation_benchmark.r")


tic("real_data_raw/data_1000_1")
benchmark("real_data_raw/data_1000_1",100,150,TRUE,TRUE)
toc()

tic("real_data_raw/data_1000_2")
benchmark("real_data_raw/data_1000_2",100,150,TRUE,TRUE)
toc()

tic("real_data_raw/data_1000_3")
benchmark("real_data_raw/data_1000_3",100,150,TRUE,TRUE)
toc()

tic("real_data_raw/data_25000_1")
benchmark("real_data_raw/data_25000_1",100,150,TRUE,TRUE)
toc()

tic("real_data_raw/data_25000_2")
benchmark("real_data_raw/data_25000_2",100,150,TRUE,TRUE)
toc()

tic("real_data_raw/data_50000_1")
benchmark("real_data_raw/data_50000_1",100,150,TRUE,TRUE)
toc()

tic("real_data_raw/data_50000_2")
benchmark("real_data_raw/data_50000_2",100,150,TRUE,TRUE)
toc()

tic("real_data_simulated/data_hd_1")
benchmark("real_data_simulated/data_hd_1",100,150,TRUE,TRUE)
toc()

tic("real_data_simulated/data_hd_2")
benchmark("real_data_simulated/data_hd_2",100,150,TRUE,TRUE)
toc()

tic("real_data_simulated/data_ld_1")
benchmark("real_data_simulated/data_ld_1",100,150,TRUE,TRUE)
toc()

tic("random_generated_data")
benchmark(NULL,100,150,TRUE,TRUE)
toc()
