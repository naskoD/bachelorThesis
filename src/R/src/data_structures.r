library(assertthat)

Data <- function(X,W,Y,N = length(W),N_treated=getN_treated(W)) {
  assert_that(is.numeric(X)&&is.numeric(Y)&&is.numeric(W)&&is.numeric(N))
  assert_that(is(N_treated,"numeric_treatment_dictionary"))
  
  W_bool <- rep(TRUE,times=N)
  W_bool[W==0] <- FALSE
  
  value <- list(X = X, W = W_bool, Y = Y,N=N,N_treated=N_treated)
  attr(value, "class") <- "data"
  value
}

get_index.data <- function(obj,index){
  Data(obj$X[index,],
       obj$W[index],
       obj$Y[index],
       obj$N,
       obj$N_treated)
}

getN_treated <- function(W){
  assert_that(is.numeric(W))
  
  number_treated <- sum(W)
  number_untreated <- length(W)-number_treated
  NumericTreatmentDictionary(number_treated,number_untreated)
}

Counterfactuals <-function(treated,W){
  assert_that(is(treated,"numeric_treatment_dictionary"))
  value <- list(treated=treated,observed=init_observed(treated,W),W=W)
  attr(value, "class") <- "counterfactuals"
  value
}

get_index.counterfactuals <- function(obj,index){
  Counterfactuals(NumericTreatmentDictionary(obj$treated$`TRUE`[index],obj$treated$`FALSE`[index]),obj$W[index])
}

TreatmentDictionary <- function(elements_true,elements_false){
  assert_that(class(elements_true)==class(elements_false))
  assert_that(length(elements_true)==length(elements_false))
  
  dictionary <- vector(mode="list", length=2)
  names(dictionary) <- c(FALSE, TRUE)
  dictionary[[1]] <- elements_false; dictionary[[2]] <- elements_true
  attr(dictionary, "class") <- "treatment_dictionary"
  dictionary
}

NumericTreatmentDictionary <- function(elements_true,elements_false){
  dictionary = TreatmentDictionary(elements_true,elements_false)
  assert_that(is.numeric(elements_true)&&is.numeric(elements_false))
  attr(dictionary, "class") <- "numeric_treatment_dictionary"
  dictionary

}
init_observed <- function(treated,W){
  assert_that(is(treated,"numeric_treatment_dictionary"))
  assert_that(is.logical(W))
  assert_that(length(W)==length(treated$`TRUE`))

  
  observed <-NumericTreatmentDictionary(treated$`TRUE`,treated$`FALSE`)
  observed$`TRUE`[W]=treated$`TRUE`[W]
  observed$`TRUE`[!W]=treated$`FALSE`[!W]
  observed$`FALSE`[W]=treated$`FALSE`[W]
  observed$`FALSE`[!W]=treated$`TRUE`[!W]
  observed
}

get_index <- function(obj,index) {
  assert_that(is.integer(index))
  UseMethod("get_index")
}

get_index.default <- function(obj,index){
  obj[index]
}

dummyData <- function(){
  # loading data
  N  = 2
  X1 = c(1,3)
  X2 = seq(0,0,length.out = N)
  X  = cbind(X1,X2)
  W  = c(1,0)
  Y  = X1+2 
  data <- Data(X,W,Y)
}


