library(assertthat)

Leaf <- function(value,id=0,bool_id=NULL){
  assert_that(is.numeric(value))
  assert_that(is.numeric(id))
  assert_that(is.logical(bool_id)||is.null(bool_id))
  
  id <-ifelse(id==0&&!is.null(bool_id),bool_vector_to_int(bool_id),id)
  leaf <- list(id=id,value=value)
  attr(leaf, "class") <- "leaf"
  leaf
}

length.leaf <- function(obj){1}
depth.leaf <- function(obj){0}

Node <- function(node_bool_id,feat_id,featval,left,right){
  assert_that(is.logical(node_bool_id))
  assert_that(is.numeric(feat_id))
  assert_that(is(left,"leaf")||is(left,"node")||is.null(left))
  assert_that(is(right,"leaf")||is(right,"node")||is.null(right))
  
  node <- list(id=bool_vector_to_int(node_bool_id),feat_id = feat_id,
               featval = featval,left=left,right = right)
  attr(node, "class") <- "node"
  node
}

length.node <- function(obj){
  length(obj$left)+length(obj$right)
}

depth.node <-function(obj){
  1 + max(depth(tree$left), depth(tree$right))
}
  

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

predict_treated_dictionary <- function(estimator_pair,X){
  assert_that(is(estimator_pair,"treatment_dictionary"))
  assert_that(is(estimator_pair$`TRUE`,"leaf"))
  assert_that(is.matrix(X))
  
  treated<-NumericTreatmentDictionary(0,0)
  for(tr in names(treated)){
    treated[[tr]]<-predict_leaf_matrix(estimator_pair[[tr]],X)
  }
  treated
}

predict_leaf_matrix <- function(leaf,X){
  assert_that(is(leaf,"leaf"))
  assert_that(is.matrix(X))
  
  N = length(X[,1])
  predictions <-rep(predict_leaf(leaf),times=N)
  predictions
} 


predict_leaf <- function(leaf){
  assert_that(is(leaf,"leaf"))
  leaf$value
}

depth.default <- function(obj){0}
