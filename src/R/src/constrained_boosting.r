library(assertthat)

constrained_boost <- function(data,loss,synth_effect,
                              n_trees,max_depth=3,regularization = 2,
                              min_samples_leaf=1){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effect))
  assert_that(is.numeric(n_trees))
  assert_that(is.numeric(max_depth))
  assert_that(is.numeric(regularization))
  assert_that(is.numeric(min_samples_leaf))
  
  const_pair = fit_const_pair(data,synth_effect)
  
  F <-predict_counterfactuals(const_pair,data)
  
  residuals<obs_gradient(data$Y,data$W,F)
  
  
}

fit_const_pair <- function(data,synth_effect){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effect))
  
  mean <- mean(data$Y)
  dict<-TreatmentDictionary(0,0)
  for(tr in names(dict)){
    tr_logical<-as.logical(tr)
    value<-mean+(2*tr_logical-1)*synth_effect*data$N_treated[[tr]]/data$N
    dict[[tr]]<-Leaf(value)
  }
  dict
}

predict_counterfactuals <- function(estimator_pair, data){
  assert_that(is(estimator_pair,"treatment_dictionary"))
  assert_that(is(estimator_pair$`TRUE`,"leaf"))
  assert_that(is(data,"data"))
  
  Counterfactuals(predict_treated_dictionary(estimator_pair,data$X),data$W)
}

obs_gradient <- function(Y,W,F){
  assert_that(is.numeric(Y))
  assert_that(is.logical(W))
  assert_that(is(F,"counterfactuals"))
  
  dict<-NumericTreatmentDictionary(0,0)
  for(tr in names(dict)){
    tr_logical<-as.logical(tr)

    value<- neg_grad(get_elements_by_treatment(Y,W,tr_logical),
                     get_elements_by_treatment(F,W,tr_logical,TRUE))
    dict[[tr]]<-value
  }
  dict
}