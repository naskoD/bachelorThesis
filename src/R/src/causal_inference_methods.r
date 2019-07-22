library(assertthat)
library(rlearner)
library(tictoc)

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
  
  X_tr<-data$X[which(data$W),]
  X_contr<-data$X[which(!data$W),]
  
  rlasso_est_tr = predict(rlasso_fit, X_tr)
  rlasso_est_contr = predict(rlasso_fit, X_contr)
  
  m_tr<-mean(rlasso_est_tr)
  m_contr<-mean(rlasso_est_contr)
  toc()
  
  return (m_tr-m_contr)
}


r_boost<-function(data){
  tic("rboost")
  assert_that(is(data,"data"))
  
  rboost_fit = rboost(data$X, data$W*1, data$Y)
  
  X_tr<-data$X[which(data$W),]
  X_contr<-data$X[which(!data$W),]
  
  rboost_est_tr = predict(rboost_fit, X_tr)
  rboost_est_contr = predict(rboost_fit, X_contr)
  
  m_tr<-mean(rboost_est_tr)
  m_contr<-mean(rboost_est_contr)
  toc()
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