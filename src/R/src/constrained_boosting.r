library(assertthat)
library(collections)
library(MASS)
library(NlcOptim)
library(mltools)
library(data.table)

source("../src/data_structures.r")

cross_validate<- function(data,synth_effect,
                          n_trees,max_depth=3,regularization = 2,
                          min_samples_leaf=1,nfolds=5){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effect))
  assert_integer(n_trees)
  assert_integer(max_depth)
  assert_that(is.numeric(regularization))
  assert_integer(min_samples_leaf)
  assert_integer(nfolds)
  
  #training_error<-vector(mode = "list",length = nfolds)
  test_error<-vector(mode = "list",length = nfolds)
  
  n_fold_elements<-data$N*(1-1/nfolds)
  
  for(i in 1:nfolds){
    #There are two nfolds parameters in this function.The nfold of the
    #function below states that the data will be splitted into [nfolds] sets. 
    #The first of them has N/nfolds elements, the second - N*(nfolds-1)/nfolds
    #In this loop we do nfold(from my function) such splits and always take the
    #first fold for training and the second for test.
    
    fold <- folds(W,nfolds = data$N/n_fold_elements,stratified = TRUE)
    tr<-which(f==1)
    te<-which(f==2)
    
    F<-constrained_boost(data,synth_effect,
                         n_trees,max_depth,regularization,
                         min_samples_leaf,tr)
    
    #training_error[[i]]<-vector(mode ="numeric",length = (n_trees+1))
    test_error[[i]]<-vector(mode ="numeric",length = (n_trees+1))
    
    for(j in 1:(n_trees+1)){
      #training_error[[i]][[j]]<-sum(evaluate(data$Y[tr],F[[j]]$observed$`TRUE`[tr]))/length(tr)
      test_error[[i]][[j]]<-sum(evaluate(data$Y[te],F[[j]]$observed$`TRUE`[te]))/length(te)
    }
  }
  #return (list(training_error=training_error,test_error=test_error))
  return(test_error)
}

constrained_boost <- function(data,synth_effect,
                              n_trees,max_depth=3,regularization = 2,
                              min_samples_leaf=1,tr=TRUE){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effect))
  assert_integer(n_trees)
  assert_integer(max_depth)
  assert_that(is.numeric(regularization))
  assert_integer(min_samples_leaf)
  assert_that(is.logical(tr)||is.numeric(tr))
  
  const_pair = fit_const_pair(get_index(data,tr),synth_effect)
  
  F<-vector(mode="list",length = n_trees)
  F[[1]]<-predict_counterfactuals(const_pair,data)
  
  residuals<-obs_gradient(data$Y,data$W,F[[1]],ind = tr)

  for (i in 1:n_trees){
    tree_pair<-fit_tree_pair(get_index(data,tr),residuals,max_depth,min_samples_leaf)
    
    leaf_index_dict<-build_leaf_index(tree_pair,data$X)
    
    v<-step_search(data,F[[i]],leaf_index_dict,regularization)
    
    F[[i+1]]<-add_counterfactuals(F[[i]],predict_counterfactuals(tree_pair,data,v))
    
    residuals<-obs_gradient(data$Y,data$W,F[[i+1]],ind=tr)
  }
  return(F)
}

fit_tree_pair<-function(data,residuals,max_depth,min_samples_leaf){
  assert_that(is(data,"data"))
  assert_that(is(residuals,"numeric_treatment_dictionary"))
  assert_integer(max_depth)
  assert_integer(min_samples_leaf)
  
  tree_pair<-TreatmentDictionary(0,0)
  for(tr in names(tree_pair)){
    tr_logical<-as.logical(tr)
    tree<-fit_regression_tree(get_elements_by_treatment(data$X,data$W,t=tr_logical), 
                              residuals[[tr]],
                              min_samples_leaf=min_samples_leaf,
                              max_depth=max_depth)
    tree_pair[[tr]]<-tree
  }
  tree_pair
}

fit_const_pair <- function(data,synth_effect){
  assert_that(is(data,"data"))
  assert_that(is.numeric(synth_effect))
  
  mean <- mean(data$Y)
  dict<-TreatmentDictionary(0,0)
  for(tr in names(dict)){
    tr_logical<-as.logical(tr)
    value<-mean+(2*tr_logical-1)*synth_effect*data$N_treated[[tr]]/data$N
    dict[[tr]]<-Leaf(value)
  }
  dict
}

predict_counterfactuals <- function(estimator_pair, data,v=NULL){
  assert_that(is(estimator_pair,"treatment_dictionary"))
  assert_that(is(estimator_pair$`TRUE`,"leaf")||is(estimator_pair$`TRUE`,"node"))
  assert_that(is(data,"data"))
  assert_that(is.null(v)||is(v,"treatment_dictionary"))
  
  if(is.null(v)){
    return (Counterfactuals(predict_treated_dictionary(estimator_pair,data$X),data$W))
  }
  else{
    treated<-NumericTreatmentDictionary(vector(mode = "numeric",length = length(data$W)),
                                        vector(mode = "numeric",length = length(data$W)))
    for(tr in names(estimator_pair)){
      assignments<-predict_rt_matrix(estimator_pair[[tr]],data$X,TRUE)
      
      for(i in 1:length(assignments)){
        treated[[tr]][[i]]<-v[[tr]]$get(as.character(assignments[i]))
      }
    }
    return (Counterfactuals(treated,data$W))
  }
 
}


obs_gradient <- function(Y,W,F,ind=TRUE){
  assert_that(is.numeric(Y))
  assert_that(is.logical(W))
  assert_that(is(F,"counterfactuals"))
  assert_that(is.logical(ind)||is.numeric(ind))
  
  dict<-NumericTreatmentDictionary(0,0)
  for(tr in names(dict)){
    tr_logical<-as.logical(tr)

    value<- neg_grad(get_elements_by_treatment(Y[ind],W[ind],tr_logical),
                     get_elements_by_treatment(get_index(F,ind),W[ind],tr_logical,TRUE))
    dict[[tr]]<-value
  }
  dict
}

build_leaf_index<-function(tree_pair,X){
  assert_that(is(tree_pair,"treatment_dictionary"))
  assert_that(is.matrix(X)&&is.numeric(X))
  
  assignments<-predict_treated_dictionary(tree_pair,X,TRUE)
 
  assignment_index_treatment_dictionary <-TreatmentDictionary(Dict$new(),Dict$new())
  for(tr in names(assignments)){
    #Unique leaves' indeces for treatment
    leaves<-unique(assignments[[tr]])
    for(l in leaves){
      indeces<-which(assignments[[tr]]==l)
      assignment_index_treatment_dictionary[[tr]]$set(as.character(l),indeces)
    }
  }
  assignment_index_treatment_dictionary
}

step_search<-function(data, F, assignment_indeces_dict,regularization,ind=TRUE){
  assert_that(is(data,"data"))
  assert_that(is(F,"counterfactuals"))
  assert_that(is(assignment_indeces_dict,"treatment_dictionary"))
  assert_that(is.numeric(regularization))
  assert_that(is.logical(ind)||is.numeric(ind))
  
  F1 <- F$treated$`TRUE`
  F0 <- F$treated$`FALSE`
  Y <- data$Y
  W <- data$W
  
  idx1<-assignment_indeces_dict$`TRUE` 
  idx0<-assignment_indeces_dict$`FALSE`
  
  filter1<-which(W==1)
  filter0<-which(W==0)
  
  if(!is.logical(ind)){
    filter1<-intersect(filter1,ind)
    filter0<-intersect(filter0,ind)
  }
  
  idx1obs <- filter_index(idx1, filter1)
  idx0obs <-filter_index(idx0, filter0)
  
  n1<-length(idx1$keys())
  n0<-length(idx0$keys())
  
  v<-rep(0.1,times=n1+n0)
  
  objfun=function(x){
    fun<-0
    for (i in 1:length(idx1obs$keys())){
      k<-idx1obs$keys()[i]
      sum_loss_key<-0
      for(j in idx1obs$get(k)){
        sum_loss_key<-sum_loss_key+(Y[j] - F1[j] - x[i])^2
      }
      fun<- fun+sum_loss_key
    }
    for (i in 1:length(idx0obs$keys())){
      k<-idx0obs$keys()[i]
      sum_loss_key<-0
      for(j in idx0obs$get(k)){
        sum_loss_key<-sum_loss_key+(Y[j] - F0[j] - x[n1+i])^2
      }
      fun<- fun+sum_loss_key
    }
    for (i in 1:length(idx1$keys())){
      k<-idx1$keys()[i]
      sum_loss_key<-0
      for(j in idx1$get(k)){
        sum_loss_key<-sum_loss_key + regularization*x[i]^2
      }
      fun<- fun+sum_loss_key
    }
    for (i in 1:length(idx0$keys())){
      k<-idx0$keys()[i]
      sum_loss_key<-0
      for(j in idx0$get(k)){
        sum_loss_key<-sum_loss_key + regularization*x[n1+i]^2
      }
      fun<- fun+sum_loss_key
    }
    
    return(fun)
  }
  
  confun=function(x){
    fun1<-0
    for (i in 1:length(idx1$keys())){
      k<-idx1$keys()[i]
      sum_loss_key<-0
      for(j in idx1$get(k)){
        sum_loss_key<-sum_loss_key + x[i]
      }
      fun1<- fun1+sum_loss_key
    }
    fun0<-0
    for (i in 1:length(idx0$keys())){
      k<-idx0$keys()[i]
      sum_loss_key<-0
      for(j in idx0$get(k)){
        sum_loss_key<-sum_loss_key +x[n1+i]
      }
      fun0<- fun0+sum_loss_key
    }
    
    f=NULL
    f=rbind(f,fun1-fun0)
    return(list(ceq=f,c=NULL))
  }
  
  v<-solnl(v,objfun=objfun,confun=confun)$par[,1]
  vopt1<-v[1:n1]
  vopt0<-v[(n1+1):(n1+n0)]
  
  dict<-TreatmentDictionary(0,0)
  for(tr in names(dict)){
    dict[[tr]]<-Dict$new()
    if(as.logical(tr)){
      for (i in 1:length(idx1$keys())){
        k<-idx1$keys()[i]
        dict[[tr]]$set(k,vopt1[i])
      }
    }
    else{
      for (i in 1:length(idx0$keys())){
        k<-idx0$keys()[i]
        dict[[tr]]$set(k,vopt0[i])
      }
    }
  }
  dict
}

filter_index<-function(indices, include_subjects){
  filtered_index = Dict$new()
  for (k in indices$keys()){
    v_in <- intersect(indices$get(k),include_subjects)
    filtered_index$set(k,v_in) 
  }
  filtered_index
}
