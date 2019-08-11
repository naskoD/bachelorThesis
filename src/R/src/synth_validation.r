library(assertthat)
library(tictoc)

source("constrained_boosting.r")
source("utilities.r")
source("losses.r")
source("read_write_data.r")
source("data_structures.r")
source("sampling.r")
source("causal_inference_methods.r")


synth_validation<-function(data_src=NULL,N=100,n_trees=100,equal_share_tr_assignment=TRUE,
                           equal_share_tr_assignment_resampling=TRUE){
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  assert_that(is.logical(equal_share_tr_assignment_resampling))
  
  tic("loading data")
  data<-load_data(data_src,N,equal_share_tr_assignment)
  toc()
  
  tic("running methods")
  ates<-run_methods(data)
  toc()
  
  print("ates")
  print(ates)
  
  synth_effects<-create_synth_effects(ates)
  
  print("synth_effects")
  print(synth_effects)
  
  tic("generating synthetic data sets")
  synth_data_sets<-generate_synth_data_sets(data,synth_effects,n_trees,
                                             equal_share_tr_assignment_resampling)
  toc()
  
  tic("pick method")
  ind_best<-pick_best_method(synth_data_sets,synth_effects)
  toc()
  
  return (Synth_Validation_Result(ates,ind_best))
  
}

create_synth_effects<-function(ates){
  assert_that(is.numeric(ates))
  
  #synth effect parameters
  #probably won't be changed
  y=2
  Q=5
  
  med<-median(ates)
  ran<-range(ates)
  spread<-ran[2]-ran[1]
  
  lowest_synth_effect<-med-y*spread
  highest_synth_effect<-med+y*spread
  
  return (seq(lowest_synth_effect,highest_synth_effect,length.out = Q))
}

generate_synth_data_sets<-function(data,synth_effects,n_trees,equal_share_tr_assignment_resampling){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effects))
  assert_integer(n_trees)
  assert_that(is.logical(equal_share_tr_assignment_resampling))
  
  synth_data_sets<-vector(mode = "list",length = length(synth_effects))
  
  for(i in 1:length(synth_data_sets)){
    synth_data_sets[[i]]<-synth_data(data,synth_effects[i],n_trees,equal_share_tr_assignment_resampling)
  }
  
  return (synth_data_sets)
}


synth_data<-function(data,synth_effect,n_trees,equal_share_tr_assignment_resampling){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effect))
  assert_integer(n_trees)
  assert_that(is.logical(equal_share_tr_assignment_resampling))
  
  #For now those paramenters will be constant, 
  #because I don't plan to do any destinction 
  #beyond them in my thesis. Potential changes
  #on max_depth adn min_samples_leaf 
  
  N_samples = 1 #number of samples to be drawn from the synthetic distribution
  max_depth = 3 #  max tree depth
  regularization_par = 2 # regularization parameter
  min_samples_leaf = 5 # minimum number of samples per leaf in each tree
  nfolds=4
  
  # Cross-validation in order to find the best number of fitting iterations
  test_error<-cross_validate(data,synth_effect,n_trees,
                             max_depth,regularization_par,min_samples_leaf,nfolds)
  
  n_trees_min_error<-max(get_n_trees_min_error(test_error)-1,1)
  
  print("n_trees")
  print(n_trees_min_error)
  
  #fitting the model and finding the conditional means for each (X,W)
  F<-constrained_boost(data,synth_effect,n_trees_min_error,max_depth,regularization_par,min_samples_leaf)
  
  #finding residuals for the model
  residuals<- neg_grad(data$Y,F[[length(F)]]$observed$`TRUE`)
  
  return (sample_synthetic_data_sets(data,F,residuals,equal_share_tr_assignment_resampling,N_samples))
}

pick_best_method<-function(synth_data_sets,synth_effects){
  assert_that(is.vector(synth_data_sets))
  assert_that(is.numeric(synth_effects))
  
  ci_methods<-methods()
  avr_err_method<-c(0)
  n_of_data_sets_per_synth_effect<-length(synth_data_sets[[1]])
  
  for(i in 1:length(ci_methods)){
    avr_err_method[i]<-0
    for(j in 1:length(synth_effects)){
      error_synth_effect<-0
      for(k in 1:n_of_data_sets_per_synth_effect){
        error_data<-abs(synth_effects[j]-ci_methods[[i]](synth_data_sets[[j]][[k]]))
        error_synth_effect<-error_synth_effect+error_data
      }
      error_synth_effect<-error_synth_effect/n_of_data_sets_per_synth_effect
      avr_err_method[i]<-avr_err_method[i]+error_synth_effect
    }
    avr_err_method[i]<-avr_err_method[i]/length(synth_effects)
  }
  
  return (which.min(avr_err_method))
}
