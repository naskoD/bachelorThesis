library(testthat) 

source("../src/data_structures.r")

test_that("TreatmentDictionary",{
  expect_error(TreatmentDictionary(TRUE,2),
               "class(elements_true) not equal to class(elements_false)",fixed=TRUE)
  expect_error(TreatmentDictionary(rep(0.5,times=99),rep(1,times=100)),
               "length(elements_true) not equal to length(elements_false)",fixed=TRUE)
  treated = TreatmentDictionary(rep(0.5,times=100),rep(1,times=100))
  
})

test_that("NumericTreatmentDictionary",{
  expect_error(NumericTreatmentDictionary(TRUE,2),
               "class(elements_true) not equal to class(elements_false)",fixed=TRUE)
  expect_error(NumericTreatmentDictionary(3,"string"),
               "class(elements_true) not equal to class(elements_false)",fixed=TRUE)
  expect_error(NumericTreatmentDictionary(rep(TRUE,times=100),rep(1,times=100)),
               "class(elements_true) not equal to class(elements_false)",fixed=TRUE)
  expect_error(NumericTreatmentDictionary(rep(0.5,times=100),rep(FALSE,times=100)),
               "class(elements_true) not equal to class(elements_false)",fixed=TRUE)
  expect_error(NumericTreatmentDictionary(rep(0.5,times=99),rep(1,times=100)),
               "length(elements_true) not equal to length(elements_false)",fixed=TRUE)
  expect_error(NumericTreatmentDictionary("string","string"),
               "elements_true is not a numeric or integer vector")

  treated = NumericTreatmentDictionary(rep(0.5,times=100),rep(1,times=100))
  
})

test_that("init_observed",{
  W=rep(c(TRUE,FALSE),times=50)
  treated = NumericTreatmentDictionary(rep(0.5,times=100),rep(1,times=100))
  expect_error(init_observed(4,W),"is(object = treated, class2 = \"numeric_treatment_dictionary\") is not TRUE",fixed = TRUE)
  expect_error(init_observed(treated,"value"),"is.logical(W) is not TRUE",fixed = TRUE)
  expect_error(init_observed(treated,c(TRUE,FALSE)),"length(W) not equal to length(treated$`TRUE`)",fixed = TRUE)
  
  observed=init_observed(treated,W)
  
  expect_equal(observed$'TRUE'[W], (rep(0.5,times=50)))
  expect_equal(observed$'TRUE'[!W], (rep(1,times=50)))
  expect_equal(observed$'FALSE'[W], (rep(1,times=50)))
  expect_equal(observed$'FALSE'[!W], (rep(0.5,times=50)))

})

test_that("getN_treated",{
  expect_error(getN_treated("rr"),"W is not a numeric or integer vector")

  W=c(rep(c(1,0),times=20),1,1)
  N_treated = getN_treated(W)
  
  expect_equal(N_treated$`TRUE`,(22))
  expect_equal(N_treated$`FALSE`,(20))
  
})

test_that("Counterfactuals",{
  treated <-NumericTreatmentDictionary(c(1,1,1),c(2,2,2))
  W <- c(TRUE,TRUE,FALSE)
  counterfactuals <- Counterfactuals(treated,W)
  expect_equal(get_index(counterfactuals,3)$treated$`TRUE`,1)
  expect_equal(get_index(counterfactuals,3)$treated$`FALSE`,2)
  expect_equal(get_index(counterfactuals,3)$observed$`TRUE`,2)
  expect_equal(get_index(counterfactuals,3)$observed$`FALSE`,1)
  
  expect_equal(get_elements_by_treatment(counterfactuals,W,TRUE,TRUE),c(1,1))
  expect_equal(get_elements_by_treatment(counterfactuals,W,FALSE,FALSE),1)
  expect_equal(get_elements_by_treatment(counterfactuals,W,FALSE,TRUE),2)
  expect_equal(get_elements_by_treatment(counterfactuals,W,TRUE,FALSE),c(2,2))
})

test_that("get_elements_by_treatment.vector",{

  expect_equal(get_elements_by_treatment(1,TRUE,TRUE),1)
  expect_equal(get_elements_by_treatment(1,TRUE,FALSE),numeric(0))
  
  Y<-c(1,2,3,4,5)
  W<-c(TRUE,FALSE,TRUE,TRUE,TRUE)
  exp<-c(1,3,4,5)
  expect_equal(get_elements_by_treatment(Y,W,TRUE),exp)
  expect_equal(get_elements_by_treatment(Y,W,FALSE),2)
})


test_that("get_elements_by_treatment.matrix",{
  m<-rbind(c(1,2),c(3,4),c(5,6))
  W <- c(TRUE,FALSE,TRUE)
  exp <-rbind(c(1,2),c(5,6))
  expect_equal(get_elements_by_treatment(m,W,TRUE),exp)
  expect_equal(get_elements_by_treatment(m,W,FALSE),c(3,4))
})



