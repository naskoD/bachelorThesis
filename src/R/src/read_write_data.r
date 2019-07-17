library(assertthat)

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
  names(cf)[2] <- "A"
  write.csv(cf,file=sprintf("../../../data/real_data_raw/%s.csv",output_file_name),row.names=FALSE)
}

load_data<-function(data_src,N){
  
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  
  if(is.null(data_src)){
    #loading data (for now unreal, randomly generated data)
    X1 = runif(N,min = -pi,max = pi)
    X2 = seq(0,0,length.out = N)
    X  = cbind(X1,X2)
    W  = rbinom(n=N,size = 1,p=logistic(X1))
    Y  = sin(X1)+ rnorm(n = N, mean = 0,sd = 0.15) 
    return (Data(X,W,Y))
  }
  else{
    data <- read.csv(file=sprintf("../../../data/%s.csv",data_src), header=TRUE, sep=",")
    
    N_treated<-ceiling(N/2)
    N_control<-floor(N/2)
    
    treated<-sample_group(data,1,N_treated)
    control<-sample_group(data,0,N_control)

    data<- rbind(treated,control)
    
    X<-data.matrix(data[,3:length(data)])
    return (Data(X,data$A,data$Y))
  }
}
