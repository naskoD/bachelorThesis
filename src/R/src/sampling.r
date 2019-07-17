library(assertthat)

source("data_structures.r")

sample_synthetic_data<-function(data,F,residuals){
  
  assert_that(is(data,"data"))
  assert_that(is.list(F)&&is(F[[1]],"counterfactuals"))
  assert_that(is.vector(residuals)&&is.numeric(residuals))
  
  #sampling from the residuals
  sampled_residuals<-sample(residuals,data$N,replace = TRUE)
  sampled_residuals
  
  
  #sampling (X,W) where treated and control have equal share
  n_treated<-data$N_treated$`TRUE`
  
  n_control<-data$N_treated$`FALSE`
  
  draw_sample_treated<-sample(1:n_treated,n_treated,replace = TRUE)
  draw_sample_control<-sample((n_treated+1):(data$N),n_control,replace = TRUE)
  
  #merging both samples into single sample with equal shares of treated and control
  draw_sample<-c(draw_sample_treated,draw_sample_control)
  
  data_sample<-get_index(data,draw_sample)
  
  s_X<-data_sample$X
  s_W<-data_sample$W*1
  
  #applying the (X,Y) sample to the model and adding residuals
  s_Y<-get_index(F[[length(F)]],draw_sample)$observed$`TRUE`+sampled_residuals
  s_Y
  
  #combing the data
  Data(s_X,s_W,s_Y)
}

sample_group<-function(data,W,N){
  
  assert_that(is.data.frame(data))
  assert_that(W==0||W==1)
  assert_integer(N)
  
  data<-data[data$A==W,]
  indices<-sample(1:length(data$Y),N,replace = FALSE)
  return (data[indices,])
}