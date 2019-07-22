library(assertthat)

source("utilities.r")


Data <- function(X,W,Y,N = length(W),N_treated=getN_treated(W)) {
  assert_that(is.numeric(X)&&is.numeric(Y)&&is.numeric(W))
  assert_integer(N)
  assert_that(is(N_treated,"numeric_treatment_dictionary"))
  
  W_bool <- rep(TRUE,times=N)
  W_bool[W==0] <- FALSE
  
  value <- list(X = X, W = W_bool, Y = Y,N=N,N_treated=N_treated)
  attr(value, "class") <- "data"
  value
}

Data_to_data_frame<-function(data){
  assert_that(is(data,"data"))
  #TODO if needed
}

data_frame_to_Data<-function(data){
  assert_that(is.data.frame(data))
  
  X<-data.matrix(data[,3:length(data)])
  return (Data(X,data$W,data$Y))
  
}

get_index.data <- function(obj,index){
  Data(obj$X[index,],
       obj$W[index]*1,
       obj$Y[index])
}

getN_treated <- function(W){
  assert_that(is.numeric(W))
  
  number_treated <- sum(W)
  number_untreated <- length(W)-number_treated
  NumericTreatmentDictionary(number_treated,number_untreated)
}

Counterfactuals <-function(treated,W,observed=init_observed(treated,W)){
  assert_that(is(treated,"numeric_treatment_dictionary"))
  value <- list(treated=treated,observed=observed,W=W)
  attr(value, "class") <- "counterfactuals"
  value
}

Synth_Validation_Result<-function(ates_methods,index_best){
  assert_that(is.numeric(ates_methods))
  assert_integer(index_best)
  assert_that(index_best>0&&index_best<=length(ates_methods))
  
  value <- list(ate=ates_methods[index_best],ates_methods=ates_methods,index_best=index_best)
  attr(value, "class") <- "synth_validation_result"
  value
}

get_index.counterfactuals <- function(obj,index){
  Counterfactuals(NumericTreatmentDictionary(obj$treated$`TRUE`[index],obj$treated$`FALSE`[index]),obj$W[index])
}

get_elements_by_treatment.counterfactuals <- function(obj,W,t,o){
  
  if(is.null(o)&&is.null(t)){
    stop("No conditions provided")
  }
  else if(is.null(o)){
    obj$treated[[as.character(t)]]
  }
  else if(is.null(t)){
    obj$observed[[as.character(o)]]
  }
  else{
    if(t){
      obj$observed[[as.character(o)]][W]
    }
    else{
      obj$observed[[as.character(o)]][!W]
    }
  }
}

add_counterfactuals<-function(c1,c2){
  
  assert_that(is(c1,"counterfactuals"))
  assert_that(is(c2,"counterfactuals"))
  assert_that(length(c1$W)==length(c2$W))
  
  treated<-NumericTreatmentDictionary(0,0)
  observed<-NumericTreatmentDictionary(0,0)
  
  for(tr in names(treated)){
    treated[[tr]]<-c1$treated[[tr]]+c2$treated[[tr]]
    observed[[tr]]<-c1$observed[[tr]]+c2$observed[[tr]]
  }
  
  return(Counterfactuals(treated,c1$W,observed))
  
}

TreatmentDictionary <- function(elements_true,elements_false){
  assert_that(all(class(elements_true)==class(elements_false)))
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
  UseMethod("get_index")
}

get_index.default <- function(obj,index){
  obj[index]
}

get_elements_by_treatment <- function(obj,W,t=NULL,o=NULL) {
  
  assert_that(is.vector(obj)||is.matrix(obj)||is(obj,"counterfactuals"))
  
  assert_that(is.logical(W))              
  assert_that(is.logical(t)||is.null(t))
  assert_that(is.logical(o)||is.null(o))
  
  UseMethod("get_elements_by_treatment")
}

get_elements_by_treatment.matrix <- function(obj,W,t=TRUE){
  if(t){
    obj[W,]
  }
  else{
    obj[!W,]
  }
}

get_elements_by_treatment.default<-function(obj,W,t=TRUE){
  get_elements_by_treatment.vector(obj,W,t)
}


get_elements_by_treatment.vector <- function(obj,W,t=TRUE){
  if(t){
    obj[W]
  }
  else{
    obj[!W]
  }
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


