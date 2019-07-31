library(assertthat)
library(rlearner)
library(tictoc)
library(grf)

source("data_structures.r")

methods<-function(){
  #TODO add all methods here 
  return (c(raw_method,r_boost,r_lasso))
}

methods_names<-function(){
  return (c("raw","r_boost","r_lasso"))
}

run_methods<-function(data){
  assert_that(is(data,"data"))
  
  ates<-0
  for(i in 1:length(methods())){
    ates[i]<-methods()[[i]](data)
  }
  
  return (ates)
}

r_lasso<-function(data){
  tic("rlasso")
  assert_that(is(data,"data"))
  
  rlasso_fit = rlasso(data$X, data$W*1, data$Y)
  
  predictions<-predict(rlasso_fit)
  
  ate<-ate_from_predictions(predictions,data$W)
  
  toc()
  
  return(ate)
}


r_boost<-function(data){
  tic("rboost")
  assert_that(is(data,"data"))
  
  rboost_fit = rboost(data$X, data$W*1, data$Y)
  predictions<-predict(rboost_fit)
  
  ate<-ate_from_predictions(predictions,data$W)
  
  toc()
  
  return(ate)
}

causal_forest_ate<-function(data){
  tic("causal_forest")
  assert_that(is(data,"data"))
  
  c_forest = causal_forest(data$X, data$Y, data$W*1)
  
  predictions<-predict(c_forest)[["predictions"]]
  
  ate<-ate_from_predictions(predictions,data$W)
  
  toc()
  return (ate)
}

ate_from_predictions<-function(predictions,W){
  assert_that(is.numeric(predictions))
  assert_that(is.logical(W))
  assert_that(length(predictions)==length(W))
  
  ind_tr<-which(d$W)
  ind_contr<-which(!d$W)
  
  est_tr = predictions[ind_tr]
  est_contr = predictions[ind_contr]
  
  m_tr<-mean(est_tr)
  m_contr<-mean(est_contr)
  return (m_tr-m_contr)
}

raw_method<-function(data){
  tic("raw_method")
  assert_that(is(data,"data"))
  
  mean_treated<-mean(data$Y[data$W])
  mean_control<-mean(data$Y[!data$W])
  
  ate<-mean_treated-mean_control
  toc()
  return (ate)
}