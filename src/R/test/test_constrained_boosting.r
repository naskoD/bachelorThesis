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
  
  
  estimator_pair <- TreatmentDictionary(Leaf(5),Leaf(6))
  
  expect_error(predict_counterfactuals(estimator_pair,data$X),
               "is(object = data, class2 = \"data\") is not TRUE",fixed=TRUE)
  
  counterfactuals = predict_counterfactuals(estimator_pair,data)
  
  expect_equal(length(counterfactuals$treated$'TRUE'),2)
  expect_equal(counterfactuals$treated$'TRUE'[1],5)
  
  expect_equal(length(counterfactuals$treated$'FALSE'),2)
  expect_equal(counterfactuals$treated$'FALSE'[1],6)
})

test_that("obs_gradient",{
  
  estimator_pair <- TreatmentDictionary(Leaf(5),Leaf(6))
  data <- dummyData()
  counterfactuals = predict_counterfactuals(estimator_pair,data)
  
  residuals <- obs_gradient(data$Y,data$W,counterfactuals)
  
  expect_equal(residuals$`TRUE`,-2)
  expect_equal(residuals$`FALSE`,-1)
})
