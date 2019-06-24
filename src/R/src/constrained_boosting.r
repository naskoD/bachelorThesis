library(assertthat)

constrained_boost <- function(data,loss,synth_effect,
                              n_trees,max_depth=3,regularization = 2,
                              min_samples_leaf=1){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effect))
  
  const_pair = fit_const_pair(data,synth_effect)
  
  
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
  assert_that(is(data,"data"))
  
  Counterfactuals(,data$W)
}