library(testthat) 

source("../src/data_structures.r")
source("../src/estimators.r")
source("../src/losses.r")
source("../src/constrained_boosting.r")


test_that("fit_const_pair",{

  data <- dummyData()
  expect_error(fit_const_pair(4,4),
               "is(object = data, class2 = \"data\") is not TRUE",fixed=TRUE)
  expect_error(fit_const_pair(data,"string"),
               "synth_effect is not a numeric or integer vector",fixed=TRUE)
  
  leaf_dictionary = fit_const_pair(data,2)
  expect_equal(leaf_dictionary$"FALSE"$value,3)
  expect_equal(leaf_dictionary$"TRUE"$value,5)

  leaf_dictionary = fit_const_pair(data,-4)
  expect_equal(leaf_dictionary$"FALSE"$value,6)
  expect_equal(leaf_dictionary$"TRUE"$value,2)
})

test_that("predict_counterfactuals",{
  
  estimator_pair <- TreatmentDictionary(5,6)
  data <- dummyData()
  
  expect_error(predict_counterfactuals(4,data),
               "is(object = estimator_pair, class2 = \"treatment_dictionary\") is not TRUE",fixed=TRUE)
  
  expect_error(predict_counterfactuals(estimator_pair,data),
               "is(object = estimator_pair$`TRUE`, class2 = \"leaf\") is not TRUE",fixed=TRUE)
  
  
  estimator_pair <- TreatmentDictionary( Leaf(5,bool_id = c(TRUE,TRUE)),
                                         Leaf(6,bool_id = c(FALSE,TRUE)))
  
  expect_error(predict_counterfactuals(estimator_pair,data$X),
               "is(object = data, class2 = \"data\") is not TRUE",fixed=TRUE)
  
  counterfactuals = predict_counterfactuals(estimator_pair,data)
  
  expect_equal(length(counterfactuals$treated$'TRUE'),2)
  expect_equal(counterfactuals$treated$'TRUE'[1],5)
  
  expect_equal(length(counterfactuals$treated$'FALSE'),2)
  expect_equal(counterfactuals$treated$'FALSE'[1],6)
  
  v<-TreatmentDictionary(Dict$new(),Dict$new())
  v$`TRUE`$set("2",0.2)
  v$`TRUE`$set("3",0.4)
  v$`FALSE`$set("2",0.6)
  v$`FALSE`$set("3",0.8)
  
  c<-predict_counterfactuals(estimator_pair,data,v)
  expect_equal(c$treated$`TRUE`,c(0.4,0.4))
  expect_equal(c$treated$`FALSE`,c(0.6,0.6))
})

test_that("obs_gradient",{
  
  estimator_pair <- TreatmentDictionary(Leaf(5),Leaf(6))
  data <- dummyData()
  counterfactuals = predict_counterfactuals(estimator_pair,data)
  
  residuals <- obs_gradient(data$Y,data$W,counterfactuals)
  
  expect_equal(residuals$`TRUE`,-2)
  expect_equal(residuals$`FALSE`,-1)
})

test_that("build_leaf_index",{
  X1<-101:200
  X2<-rep(0,times=100)
  X<-cbind(X1,X2)
  
  tree<-Node(c(TRUE),1,151,
             Node(c(TRUE,TRUE),1,126,
                  Leaf(10,bool_id = c(TRUE,TRUE,TRUE)),
                  Leaf(20,bool_id = c(FALSE,TRUE,TRUE))),
             Leaf(30,bool_id = c(FALSE,TRUE)))
  
  tree_pair<-TreatmentDictionary(tree,tree)
  
  dict<-build_leaf_index(tree_pair,X)
  expect_equal(dict$`TRUE`,dict$`FALSE`)
  expect_equal(dict$`TRUE`$keys(),c("2","6","7"))
  expect_equal(dict$`TRUE`$get("2"),51:100)
  expect_equal(dict$`TRUE`$get("6"),26:50)
  expect_equal(dict$`TRUE`$get("7"),1:25)
})

test_that("filter_index",{
  indices<-Dict$new()
  indices$set("1",1:50)
  indices$set("2",51)
  indices$set("3",52:75)
  indices$set("4",76:100)
  
  filtered<-filter_index(indices,which(1:100%%2==0))
  
  expect_equal(length(filtered$keys()),4)
  expect_equal(filtered$get("1"),seq(2,50,by = 2))
  expect_equal(filtered$get("2"),numeric(0))
  expect_equal(filtered$get("3"),seq(52,74,by = 2))
  expect_equal(filtered$get("4"),seq(76,100,by = 2))
  
  filtered<-filter_index(indices,c(1,100))
  expect_equal(length(filtered$keys()),4)
  expect_equal(filtered$get("1"),1)
  expect_equal(filtered$get("2"),numeric(0))
  expect_equal(filtered$get("3"),numeric(0))
  expect_equal(filtered$get("4"),100)
})

test_that("constrained_boost",{
  
  N  = 100
  X1 = runif(N,min = -pi,max = pi)
  X2 = seq(0,0,length.out = N)
  X  = cbind(X1,X2)
  W  = rbinom(n=N,size = 1,p=logistic(X1))
  Y  = sin(X1)+ rnorm(n = N, mean = 0,sd = 0.15) 
  data <- Data(X,W,Y)
  
  # parameters for constrained boosting
  synth_effect = -2 # the synthetic effect you want to have
  n_trees=10 # max number of trees to use for constrained boosting
  three_depth = 2 #  max tree depth
  regularization_par = 2 # regularization parameter
  s = 10 # minimum number of samples per leaf in each tree
  
  F<-constrained_boost(data,synth_effect,
                       n_trees,three_depth,regularization_par,
                       min_samples_leaf=s)
  for(i in 1:(n_trees+1)){
    data_synth_effect<-(sum(F[[i]]$treated$`TRUE`)-sum(F[[i]]$treated$`FALSE`))/N
    expect_equal(data_synth_effect,synth_effect)
  }
})


