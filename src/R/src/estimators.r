library(assertthat)

Leaf <- function(value,id=0,bool_id=NULL){
  assert_that(is.numeric(value))
  
  id <-ifelse(id==0&&!is.null(bool_id),bool_vector_to_int(bool_id),id)
  leaf <- list(id=id,value=value)
  attr(leaf, "class") <- "leaf"
  leaf
}

length.leaf <- function(obj){1}
depth.leaf <- function(obj){0}


bool_vector_to_int <- function(bool_vect){
  assert_that(is.logical(bool_vect))
  
  res <- 0
  counter <- 0
  for (v in bool_vect){
    res= res + (2^counter)*v
    counter = counter+1
  } 
  res
}

depth <- function(obj) {
  UseMethod("depth")
}

predict_dictionary <- function(){
  
}

depth.default <- function(obj){0}
