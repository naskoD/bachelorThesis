library(assertthat)

evaluate <- function(Y, F){
  assert_loss(Y,F)
  (Y-F)^2
}

neg_grad <- function(Y, F){
  assert_loss(Y,F)
  (Y-F)
}

effect <- function(Ft, Ff){
  assert_loss(Ft,Ff)
  mean(Ft-Ff)
}

assert_loss <- function(Y, F){
  assert_that(is.numeric(Y))
  assert_that(is.numeric(F))
  assert_that(length(Y)==length(F))
}

