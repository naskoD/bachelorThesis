library(psych)

source("constrained_boosting.r")
source("utilities.r")
source("losses.r")

#loading data (for now unreal, randomly generated data)
N  = 100
X1 = runif(N,min = -pi,max = pi)
X2 = seq(0,0,length.out = N)
X  = cbind(X1,X2)
W  = rbinom(n=N,size = 1,p=logistic(X1))
Y  = sin(X1)+ rnorm(n = N, mean = 0,sd = 0.15) 
data <- Data(X,W,Y)

# parameters for constrained boosting
synth_effect = -2 # the synthetic effect you want to have (for now just given, latter it should be picked by the causal inference methods)
n_trees=5 # max number of trees to use for constrained boosting
max_depth = 2 #  max tree depth
regularization_par = 2 # regularization parameter
min_samples_leaf = 10 # minimum number of samples per leaf in each tree
nfolds=4

# Cross-validation in order to find the best number of fitting iterations
test_error<-cross_validate(data,synth_effect,n_trees,
                       max_depth,regularization_par,min_samples_leaf,nfolds)

n_trees_min_error<-get_n_trees_min_error(test_error)-1

#fitting the model and finding the conditional means for each (X,W)
F<-constrained_boost(data,synth_effect,n_trees_min_error,max_depth,regularization_par,min_samples_leaf)

#finding residuals for the model
residuals<- neg_grad(data$Y,F[[n_trees_min_error+1]]$observed$`TRUE`)
residuals

#sampling from the residuals
sampled_residuals<-sample(residuals,N,replace = TRUE)
sampled_residuals

#sampling (X,W)
draw_sample<-sample(1:N,N,replace = TRUE)

data_sample<-get_index(data,draw_sample)

s_X<-data_sample$X
s_W<-data_sample$W*1

#applying the (X,Y) sample to the model and adding residuals
s_Y<-get_index(F[[n_trees_min_error+1]],draw_sample)$observed$`TRUE`+sampled_residuals
s_Y

#combing the data
Data(s_X,s_W,s_Y)

#applying causal inference methods to synthetic data

