library(assertthat)

is.wholenumber <- function(x){
  assert_that(is.numeric(x)&&length(x)==1)
  x%%1==0
}

assert_integer<-function(x){
  assert_that(is.wholenumber(x),msg = paste(x, "is not an integer"))
}