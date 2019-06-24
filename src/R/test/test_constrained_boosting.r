library(testthat) 

source("../src/data_structures.r")
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
