library(assertthat)

source("data_structures.r")
source("utilities.r")

sample_synthetic_data<-function(data,F,residuals,equal_share_tr_assignment_resampling){
  
  assert_that(is(data,"data"))
  assert_that(is.list(F)&&is(F[[1]],"counterfactuals"))
  assert_that(is.vector(residuals)&&is.numeric(residuals))
  assert_that(is.logical(equal_share_tr_assignment_resampling))
  
  #sampling from the residuals
  sampled_residuals<-sample(residuals,data$N,replace = TRUE)
  sampled_residuals
  
  if(equal_share_tr_assignment_resampling){
    draw_sample<-sample_equal_share_indices(data,data$N,TRUE)
  }
  else{
    draw_sample<-sample(1:data$N,data$N,replace = TRUE)
  }
  
  data_sample<-get_index(data,draw_sample)
  
  s_X<-data_sample$X
  s_W<-data_sample$W*1
  
  #applying the (X,Y) sample to the model and adding residuals
  s_Y<-get_index(F[[length(F)]],draw_sample)$observed$`TRUE`+sampled_residuals
  s_Y
  
  #combing the data
  Data(s_X,s_W,s_Y)
}

sample_synthetic_data_sets<-function(data,F,residuals,equal_share_tr_assignment_resampling,
                                     N_samples){
  assert_that(is(data,"data"))
  assert_that(is.list(F)&&is(F[[1]],"counterfactuals"))
  assert_that(is.vector(residuals)&&is.numeric(residuals))
  assert_that(is.logical(equal_share_tr_assignment_resampling))
  assert_integer(N_samples)
  
  synth_data_sets<-vector(mode = "list",length = N_samples)
  
  for(i in 1:N_samples){
    synth_data_sets[[i]]<-sample_synthetic_data(data,F,residuals,equal_share_tr_assignment_resampling)
  }
  
  return (synth_data_sets)
}

sample_equal_share_indices<-function(data,N,replace){
  assert_that(is.data.frame(data)||is(data,"data"))
  assert_integer(N)
  
  #equal number of observations in each group
  N_treated<-ceiling(N/2)
  N_control<-floor(N/2)
  
  W_int<-data$W*1
  #finding treated and control indices
  ind_treated<-which(W_int==1)
  ind_control<-which(W_int==0)
  
  #sampling from this indices
  if(length(ind_treated)!=1){
    #sample samples from 1:x, if one element
    ind_treated<-sample(ind_treated,N_treated,replace = replace)
    ind_control<-sample(ind_control,N_control,replace = replace) 
  }
  
  #getting them together and permutating them 
  indices<-sample(c(ind_treated,ind_control),N,replace = FALSE)
  
  return (indices)
}

sample_equal_share_data<-function(data,N){
  
  assert_that(is.data.frame(data))
  assert_integer(N)
  
  indices<-sample_equal_share_indices(data,N,FALSE)
  return (data[indices,])
}

sample_equal_share_generated_assignments<-function(N){
  assert_integer(N)
  
  N_treated<-ceiling(N/2)
  N_control<-floor(N/2)
  
  W<-sample(c(rep(1,times=N_treated),rep(0,times=N_control)),N,replace = FALSE)
  return (W)
}