library(testthat) 

source("../src/estimators.r")

test_that("Leaf",{
  
  expect_error(Leaf("not number"),"value is not a numeric or integer vector")
  
  leaf = Leaf(5)
  expect_equal(leaf$id,(0))
  expect_equal(leaf$value,(5))
  
  leaf = Leaf(10,2)
  expect_equal(leaf$id,(2))
  expect_equal(leaf$value,(10))
  
  leaf = Leaf(10,2,bool_id = c(TRUE,FALSE,TRUE))
  expect_equal(leaf$id,(2))
  expect_equal(leaf$value,(10))
  
  leaf = Leaf(10,bool_id = c(TRUE,FALSE,TRUE))
  expect_equal(leaf$id,(5))
  expect_equal(leaf$value,(10))
  expect_equal(length(leaf),(1))
  expect_equal(depth(leaf),(0))
  
})

test_that("Test bool_vector_to_int",{
  expect_equal(bool_vector_to_int(TRUE), (1))
  expect_equal(bool_vector_to_int(FALSE), (0))
  expect_equal(bool_vector_to_int(c(FALSE,FALSE)), (0))
  expect_equal(bool_vector_to_int(c(TRUE,TRUE)), (3))
  expect_equal(bool_vector_to_int(c(TRUE,TRUE,FALSE)), (3))
  expect_equal(bool_vector_to_int(rep(TRUE,times=10)), (2^10-1))
  expect_error(bool_vector_to_int(2),"is.logical(bool_vect) is not TRUE",fixed=TRUE)
  expect_error(bool_vector_to_int(c(2,TRUE,FALSE)),"is.logical(bool_vect) is not TRUE",fixed=TRUE)
})