library(testthat) 

source("../src/utilities.r")

test_that("is.wholenumber",{
  expect_true(is.wholenumber(-1))
  expect_true(is.wholenumber(0))
  expect_true(is.wholenumber(1))
  expect_true(is.wholenumber(10000000000))
  
  expect_false(is.wholenumber(0.1))
  expect_false(is.wholenumber(-0.1))
  
  expect_error(is.wholenumber("w"))
  expect_error(is.wholenumber(c(0,1,2,3)))
  expect_error(is.wholenumber(TRUE))
})