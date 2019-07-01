library(psych)

source("../src/constrained_boosting.r")

# loading data
N  = 100
X1 = runif(N,min = -pi,max = pi)
X2 = seq(0,0,length.out = N)
X  = cbind(X1,X2)
W  = rbinom(n=N,size = 1,p=logistic(X1))
Y  = sin(X1)+ rnorm(n = N, mean = 0,sd = 0.15) 
data <- Data(X,W,Y)
# parameters for constrained boosting
synth_effect = -2 # the synthetic effect you want to have
n_trees=100 # max number of trees to use for constrained boosting
max_depth = 2 #  max tree depth
regularization_par = 2 # regularization parameter
min_samples_leaf = 10 # minimum number of samples per leaf in each tree
nfolds=4

test_error<-cross_validate(data,synth_effect,n_trees,
                       max_depth,regularization_par,min_samples_leaf,nfolds)

n_trees_min_error<-get_n_trees_min_error(test_error)-1


F<-constrained_boost(data,synth_effect,n_trees_min_error,three_depth,regularization_par,min_samples_leaf)

F

get_n_trees_min_error<-function(test_error){
  error_matrix<-NULL
  
  for(i in 1:length(test_error)){
    error_matrix<-rbind(error_matrix,test_error[[i]])
  }
  
  means<-vector(mode = "numeric",length = length(error_matrix[1,]))
  
  for(i in 1:length(means)){
    means[[i]]<-mean(error_matrix[,i])
  }
  
  return(which.min(means))
  
}