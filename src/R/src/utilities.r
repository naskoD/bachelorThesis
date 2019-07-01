library(assertthat)

is.wholenumber <- function(x){
  assert_that(is.numeric(x)&&length(x)==1)
  x%%1==0
}

assert_integer<-function(x){
  assert_that(is.wholenumber(x),msg = paste(x, "is not an integer"))
}


are_elements_equal<- function(x,tol=sqrt(.Machine$double.eps)){
  assert_that(is.numeric(x))
  if (length(x) == 1) return(TRUE)
  abs(max(x) - min(x)) < tol
}
  