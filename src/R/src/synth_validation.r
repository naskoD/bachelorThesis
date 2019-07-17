library(psych)
library(assertthat)

source("constrained_boosting.r")
source("utilities.r")
source("losses.r")
source("read_write_data.r")
source("data_structures.r")
source("sampling.r")

synth_data<-function(data_src=NULL,N=100){
  
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  
  data<-load_data(data_src,N)
  
  # parameters for constrained boosting
  synth_effect = -2 # the synthetic effect you want to have (for now just given, latter it should be picked by the causal inference methods)
  n_trees=2 # max number of trees to use for constrained boosting TODO set it to 100
  max_depth = 2 #  max tree depth
  regularization_par = 2 # regularization parameter
  min_samples_leaf = 5 # minimum number of samples per leaf in each tree
  nfolds=4
  
  # Cross-validation in order to find the best number of fitting iterations
  test_error<-cross_validate(data,synth_effect,n_trees,
                             max_depth,regularization_par,min_samples_leaf,nfolds)
  
  n_trees_min_error<-get_n_trees_min_error(test_error)-1
  
  #fitting the model and finding the conditional means for each (X,W)
  F<-constrained_boost(data,synth_effect,n_trees_min_error,max_depth,regularization_par,min_samples_leaf)
  
  #finding residuals for the model
  residuals<- neg_grad(data$Y,F[[n_trees_min_error+1]]$observed$`TRUE`)
  residuals
  
  sample_synthetic_data(data,F,residuals)
  
  #applying causal inference methods to synthetic data
}
synth_data("real_data_raw/data_25000_1",100)
synth_data()
