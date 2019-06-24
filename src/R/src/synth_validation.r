library(psych)

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
three_depth = 2 #  max tree depth
regularization_par = 2 # regularization parameter
s = 10 # minimum number of samples per leaf in each tree

