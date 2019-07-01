library(testthat) 

source("../src/estimators.r")

test_that("Leaf",{
  
  expect_error(Leaf("not number"),"value is not a numeric or integer vector")
  
  leaf = Leaf(5)
  expect_equal(leaf$id,(0))
  expect_equal(leaf$value,(5))
  
  leaf = Leaf(10,2)
  expect_equal(leaf$id,(2))
  expect_equal(leaf$value,(10))
  
  leaf = Leaf(10,2,bool_id = c(TRUE,FALSE,TRUE))
  expect_equal(leaf$id,(2))
  expect_equal(leaf$value,(10))
  
  leaf = Leaf(10,bool_id = c(TRUE,FALSE,TRUE))
  expect_equal(leaf$id,(5))
  expect_equal(leaf$value,(10))
  expect_equal(length(leaf),(1))
  expect_equal(depth(leaf),(0))
  
})

test_that("Node",{
  
  node <- Node(c(TRUE,TRUE),4,7,Leaf(4),Leaf(5))
  expect_equal(node$id,3)
  expect_equal(node$feat_id,4)
  expect_equal(node$featval,7)
  expect_equal(node$left$value,4)
  expect_equal(node$right$value,5)
  expect_equal(length(node),2)
  expect_equal(depth(node),1)
  
  node2 <- Node(c(TRUE,FALSE),4,7,node,NULL)
  expect_equal(node2$id,1)
  expect_equal(node2$feat_id,4)
  expect_equal(node2$featval,7)
  expect_equal(node2$left$id,3)
  expect_equal(node2$left$left$value,4)
  expect_equal(node2$left$right$value,5)
  expect_equal(node2$right,NULL)
  expect_equal(length(node2),2)
  expect_equal(depth(node2),2)
  
  node2$right<- node
  expect_equal(length(node2),4)
  expect_equal(depth(node2),2)
})

test_that("predict_rt",{
  leaf <- Leaf(6)
  
  expect_error(predict_rt("dd",6),paste("is(object = tree, class2 = \"leaf\") is not",
               "TRUE or is(object = tree, class2 = \"node\") is not TRUE"),
               fixed=TRUE)
  expect_equal(predict_rt(leaf,1),6)
  
  node1 <- Node(c(TRUE,TRUE),1,NULL,Leaf(4),Leaf(5))
  
  expect_equal(predict_rt(node1,1),4)
  expect_equal(predict_rt(node1,1,TRUE),0)
  expect_equal(predict_rt(node1,1,FALSE),4)
  
  node2 <- Node(c(TRUE,TRUE),1,7,Leaf(4),Leaf(5))
  
  expect_equal(predict_rt(node2,1),4)
  expect_equal(predict_rt(node2,7),5)
  expect_equal(predict_rt(node2,10),5)
  expect_equal(predict_rt(node2,1,TRUE),0)
  expect_equal(predict_rt(node2,1,FALSE),4)
  
  node3 <- Node(c(TRUE,TRUE),2,7,Leaf(4,bool_id = c(TRUE)),Leaf(5))
  expect_equal(predict_rt(node3,c(1,1)),4)
  expect_equal(predict_rt(node3,c(1,10)),5)
  expect_equal(predict_rt(node3,c(1,1),TRUE),1)
  expect_equal(predict_rt(node3,c(1,1),FALSE),4)
  
  node4 <- Node(c(TRUE,TRUE),1,10,node2,Leaf(100,bool_id = c(TRUE,FALSE,TRUE)))
  expect_equal(predict_rt(node4,1),4)
  expect_equal(predict_rt(node4,7),5)
  expect_equal(predict_rt(node4,9),5)
  expect_equal(predict_rt(node4,10),100)
  expect_equal(predict_rt(node4,11),100)
  expect_equal(predict_rt(node4,50),100)
  expect_equal(predict_rt(node4,50,TRUE),5)
  expect_equal(predict_rt(node4,50,FALSE),100)
})

test_that("predict_rt_matrix",{
  
  tree<-Leaf(101)
  matrix <- rbind(c(1,2),c(3,4),c(5,6),c(7,8),c(9,10))
  values <- predict_rt_matrix(tree,matrix)
  
  expect_equal(length(values),5)
  expect_equal(values[1],101)
  
  tree<-Node(c(TRUE,TRUE),1,7,Leaf(4),Leaf(5))
  values <- predict_rt_matrix(tree,matrix)
  
  expect_equal(length(values),5)
  expect_equal(values[1:3],c(4,4,4))
  expect_equal(values[4:5],c(5,5))
})

test_that("predict_treated_dictionary",{
  estimator_pair<-TreatmentDictionary(5,6)
  
  expect_error(predict_treated_dictionary("string"),"is(object = estimator_pair, class2 = \"treatment_dictionary\") is not TRUE",
               fixed=TRUE)
  
  estimator_pair<-TreatmentDictionary(Leaf(100),Leaf(200))
  matrix <- rbind(c(1,2),c(3,4),c(5,6),c(7,8),c(9,10))
  
  treated <-predict_treated_dictionary(estimator_pair,matrix)
  
  expect_equal(length(treated$`FALSE`),5)
  expect_equal(treated$`FALSE`[1],200)
  expect_equal(length(treated$`TRUE`),5)
  expect_equal(treated$`TRUE`[1],100)
})

test_that("best_mse_loss",{
  X<-c(1,2,3,4,5)
  Y<-c(1,2,3,4,5)
  mse<-best_mse_loss(X,Y,X)
  
  expect_equal(mse[1],-2.5)
  expect_equal(mse[2],3)
  
  
  X<-c(1,2,3,4,5,6,7,8,9)
  Y<-c(0,0,0,0,0,0,0,0,5)
  mse<-best_mse_loss(X,Y,X)
  
  expect_equal(mse[1],0)
  expect_equal(mse[2],9)
  
})

test_that("split_mse",{
  
  X1<-c(rep(0,times=999),1)
  X2<-1:1000
  X<-cbind(X1,X2)
  Y<-1:1000
  S<-split_mse(X,Y)
  
  expect_equal(S[1],2)
  expect_equal(S[2],500.5)
  
  
  X1<-1:100
  X2<-c(rep(0,times=99),1)
  X<-cbind(X1,X2)
  Y<-seq(3,2314,length.out = 100)
  S<-split_mse(X,Y)
  
  expect_equal(S[1],1)
  expect_equal(S[2],51)
})


test_that("fit_regression_tree",{
  X1<-1:3
  X2<-rep(0,times=3)
  X<-cbind(X1,X2)
  Y<- X1+2
  
  tree<-fit_regression_tree(X,Y)
  
  expect_true(is(tree,"leaf"))
  expect_equal(tree$id,1)
  expect_equal(tree$value,4)
  expect_equal(predict_rt(tree,c(1,0)),4)
  expect_equal(predict_rt(tree,c(1,0),TRUE),1)
  
  X1<-1:1000
  X2<-rep(0,times=1000)
  X<-cbind(X1,X2)
  Y<- c(rep(0,times=999),1)

  tree<-fit_regression_tree(X,Y)

  expect_true(is(tree,"node"))
  expect_equal(tree$id,1)
  expect_equal(tree$featval,1000)
  
  expect_equal(predict_rt(tree,c(100,0)),0)
  expect_equal(predict_rt(tree,c(100,0),TRUE),3)
  
  expect_equal(predict_rt(tree,c(1000,0)),1)
  expect_equal(predict_rt(tree,c(1000,0),TRUE),2)
  
  expect_true(is(tree$left,"leaf"))
  expect_equal(tree$left$id,3)
  expect_equal(tree$left$value,0)
  
  expect_true(is(tree$right,"leaf"))
  expect_equal(tree$right$id,2)
  expect_equal(tree$right$value,1)
  
})


test_that("Test bool_vector_to_int",{
  expect_equal(bool_vector_to_int(TRUE), (1))
  expect_equal(bool_vector_to_int(FALSE), (0))
  expect_equal(bool_vector_to_int(c(FALSE,FALSE)), (0))
  expect_equal(bool_vector_to_int(c(TRUE,TRUE)), (3))
  expect_equal(bool_vector_to_int(c(TRUE,TRUE,FALSE)), (3))
  expect_equal(bool_vector_to_int(rep(TRUE,times=10)), (2^10-1))
  expect_error(bool_vector_to_int(2),"is.logical(bool_vect) is not TRUE",fixed=TRUE)
  expect_error(bool_vector_to_int(c(2,TRUE,FALSE)),"is.logical(bool_vect) is not TRUE",fixed=TRUE)
})