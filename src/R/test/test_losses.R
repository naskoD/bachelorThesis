library(testthat) 

source("../src/losses.r")

test_that("evaluate",{
  
  Y = c(1,2,3,4,5)
  F = c(6,7,8,9,10)
  expect_equal(evaluate(Y,F),c(25,25,25,25,25))
  
})

test_that("neg_grad",{
  
  Y = c(1,2,3,4,5)
  F = c(6,7,8,9,10)
  expect_equal(neg_grad(Y,F),c(-5,-5,-5,-5,-5))
  
})

test_that("effect",{
  
  Ft = c(1,2,3,4,5)
  Ff = c(6,7,8,9,10)
  expect_equal(effect(Ft,Ff),-5)
  
})