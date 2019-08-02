library(assertthat)
library(psych)

source("data_structures.r")
source("sampling.r")

calculate_ate_from_cf_and_write_it_to_file<-function(data_name){
  assert_that(is.character(data_name))
  
  cf <- read.csv(file=sprintf("../../../data/real_data_raw/%s_cf.csv",data_name),
                     header=TRUE, sep=",")
  
  ate<- (sum(cf$y1)-sum(cf$y0))/length(cf$y0)
  
  write(ate,sprintf("../../../data/real_data_raw/%s_ate.txt",data_name) )
  ate
}

extract_covariates_into_data_file<-function(input_file_name,output_file_name){
  assert_that(is.character(input_file_name))
  assert_that(is.character(output_file_name))
  
  cf <- read.csv(file=sprintf("../../../data/real_data_raw/%s.csv",input_file_name),
                 header=TRUE, sep=",")
  
  all_covariates<-read.csv(file="../../../data/real_data_raw/covariates.csv",
                           header=TRUE, sep=",")
  
  cf<-merge(x = cf, y = all_covariates, by = "sample_id", all.x = TRUE)
  
  cf<-cf[2:length(cf)] #removing sample_id
  cf<-cf[c(2,1,3:length(cf))]#reorder first and second column
  names(cf)[1] <- "Y"
  names(cf)[2] <- "W"
  write.csv(cf,file=sprintf("../../../data/real_data_raw/%s.csv",output_file_name),row.names=FALSE)
}

load_data<-function(data_src,N,equal_share_tr_assignment){
  
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  
  if(is.null(data_src)){
    return (load_generated_data(equal_share_tr_assignment,N))
  }
  else{
    data <- read.csv(file=sprintf("../../../data/%s.csv",data_src), header=TRUE, sep=",")
    names(data)[2] <- "W"
    print(paste("tr:",sum(data$W),"all:",length(data$W)))
  
    if(equal_share_tr_assignment){
      data<-sample_equal_share_data(data,N)
    }
    else{
      indices<-sample(1:length(data$Y),N,replace = FALSE)
      data<-data[indices,]
    }
    return (data_frame_to_Data(data))
  }
}

load_generated_data<-function(equal_share_tr_assignment,N){
  assert_that(is.logical(equal_share_tr_assignment))
  assert_integer(N)
  
  X1 = runif(N,min = -pi,max = pi)
  X2 = seq(0,0,length.out = N)
  X  = cbind(X1,X2)
  
  if(equal_share_tr_assignment){
    W = sample_equal_share_generated_assignments(N)
  }
  else{
    W  = rbinom(n=N,size = 1,p=logistic(X1))
  }
  
  Y  = sin(X1)+ rnorm(n = N, mean = 0,sd = 0.15) 
  
  return (Data(X,W,Y))
}

load_real_ate<-function(data_src){
  assert_that(is.null(data_src)||is.character(data_src))
  
  if(is.null(data_src)){
    return (0)
  }
  
  ate<-read.table(file = sprintf("../../../data/%s_ate.txt",data_src))
  return (ate[[1]])
}

read_benchmark_data<-function(bench_data_src){
  
  assert_that(is.character(bench_data_src))
  
  data <- read.csv(file=sprintf("../../../benchmark_data/%s.csv",bench_data_src),
                   header=TRUE, sep=",")
  return (data)
}

write_benchmark_data<-function(benchmark_data,bench_data_src){
  
  assert_that(is.data.frame(benchmark_data))
  assert_that(is.character(bench_data_src))
  
  data <- write.csv(benchmark_data,file=sprintf("../../../benchmark_data/%s.csv",bench_data_src),
                    row.names=FALSE)
  data
}

create_benchmark_directory<-function(){
  setwd("../../../")
  dir.create("benchmark_data",showWarnings = FALSE)
  dir.create("benchmark_data/benchmark_random_generated_data", showWarnings = FALSE)
  dir.create("benchmark_data/benchmark_real_data_raw", showWarnings = FALSE)
  dir.create("benchmark_data/benchmark_real_data_simulated",showWarnings = FALSE)
  setwd("src/R/src")
}
