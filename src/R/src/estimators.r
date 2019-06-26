library(assertthat)

NO_BEST <-c(0,0)

Leaf <- function(value,id=0,bool_id=NULL){
  assert_that(is.numeric(value))
  assert_integer(id)
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
  assert_integer(feat_id)
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
  1 + max(depth(obj$left), depth(obj$right))
}

fit_regression_tree <- function(X,Y,min_samples_leaf=5,max_depth=-1, node_id_bool=TRUE){
  #May be needed to remove the is.vector assert
  assert_that((is.matrix(X)||is.vector())&&is.numeric(X))
  assert_that(is.vector(Y)&&is.numeric(Y))
  assert_integer(min_samples_leaf)
  assert_integer(max_depth)
  assert_integer(min_samples_leaf)
  assert_that(is.logical(node_id_bool))
  
  if (max_depth < -1){
    stop(cat("Unexpected value for max_depth: ",max_depth,
             " (expected: max_depth >= 0, or max_depth = -1 for infinite depth)"))
  }
  
  if (length(Y) <= min_samples_leaf || max_depth==0){
     Leaf(mean(Y),bool_id = node_id_bool)
  }
  else{
    S <- split_mse(X, Y)
    
    if(identical(S,NO_BEST)){
      Leaf(mean(Y),bool_id = node_id_bool)
    }
    else{
      feat_id <- S[1];thresh = S[2]
      
      split <- X[,feat_id]< thresh
      
      max_depth <- max(max_depth-1, -1)
      
      Node(node_id_bool, feat_id, thresh,
           
                  fit_regression_tree(X[split,], Y[split],min_samples_leaf, max_depth, 
                                      node_id_bool =c(TRUE,node_id_bool)),
           
                  fit_regression_tree(X[!split,],Y[!split],min_samples_leaf,max_depth, 
                                      node_id_bool =c(FALSE,node_id_bool))
      )
    }
  }
}
  

split_mse <- function(X,Y){
  
  assert_that(is.matrix(X))
  assert_that(is.vector(Y))
  
  N<-length(X[,1])
  p<-length(X[1,])
  
  best <- NO_BEST
  best_val <- -Inf
  
  for (i in 1:p){
    ord <- order(X[,i])
    X_i <- X[ord,i]
    Y_i <- Y[ord]
    
    if(N > 100){
      domain_i <- quantile(X_i,seq(0.01,0.99,by=0.01))
    }
    else{
      domain_i = X_i
    }
    #TODO
    #mse<-best_mse_loss(X_i, Y_i, domain_i)
    value <- mse[0]; thresh <- mse[1] 
    if (value > best_val){
      best_val <- value
      best<-c(i,thresh)
    }
  }
  best
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
    treated[[tr]]<-predict_rt_matrix(estimator_pair[[tr]],X)
  }
  treated
}

predict_rt_matrix <- function(tree,X){
  assert_that(is(tree,"leaf")||is(tree,"node"))
  assert_that(is.matrix(X))
  
  N = length(X[,1])
  predictions <- rep(0,times=N)
  for(i in 1:N){
    predictions[i]<-predict_rt(tree,X[i,])
  }
  predictions
} 

predict_rt <- function(tree,X){
  assert_that(is(tree,"leaf")||is(tree,"node"))
  assert_that(is.vector(X))
  if(is(tree,"leaf")){
    tree$value
  }
  else{
    if(is.null(tree$featval)){
       predict_rt(tree$left, X)
    }
    else if(X[tree$feat_id]<tree$featval){
      predict_rt(tree$left, X)
    }
    else{
      predict_rt(tree$right, X)
    }
  }
}

depth.default <- function(obj){0}
