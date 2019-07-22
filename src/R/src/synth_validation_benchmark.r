source("read_write_data.r")
source("synth_validation.r")
source("causal_inference_methods.r")
source("utilities.r")

benchmark<-function(data_src=NULL,N=100,n_trees=2,equal_share_tr_assignment=TRUE,
                    equal_share_tr_assignment_resampling=TRUE){
  
  #TODO final version will work with higher n_trees, e.g. 100
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  assert_that(is.logical(equal_share_tr_assignment))
  
  ate<-load_real_ate(data_src)
  
  b_data_src<-get_benchmark_data_src(data_src,N,n_trees,equal_share_tr_assignment,
                      equal_share_tr_assignment_resampling)

  #Trying to read benchmark data file and
  #if error => create new data frame
  b_data = tryCatch({
    df<-read_benchmark_data(b_data_src)
    data_row_c<-get_data_row_c(ate,data_src,N,n_trees,equal_share_tr_assignment,
                               equal_share_tr_assignment_resampling)
    print(data_row_c)
    
    df<-rbind(b_data,as.list(data_row_c))
    return (df)
    
  },error = function(e) {
    data_row_c<-get_data_row_c(ate,data_src,N,n_trees,equal_share_tr_assignment,
                               equal_share_tr_assignment_resampling)
    df<-data.frame(as.list(data_row_c))
    names(df)<-c("oracle","synth_validation",methods_names())
    
    return (df)
  })
  
  write_benchmark_data(b_data,b_data_src)
  
  b_data
  
  #TODO analyse file,create plots
}

get_data_row_c<-function(ate,data_src,N,n_trees,equal_share_tr_assignment,
                       equal_share_tr_assignment_resampling){
  
  assert_that(is.numeric(ate))
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  assert_that(is.logical(equal_share_tr_assignment))
  
  res<-synth_validation(data_src,N,n_trees,equal_share_tr_assignment,
                        equal_share_tr_assignment_resampling)
  
  avg_tr_errors<-sqrt((res$ates_methods-ate)^2)
  
  oracle_error<-min(avg_tr_errors)
  
  synth_valid_error<-avg_tr_errors[res$index_best]
  
  return (c(oracle_error,synth_valid_error,avg_tr_errors))
}

get_benchmark_data_src<-function(data_src,N,n_trees,equal_share_tr_assignment,
                                 equal_share_tr_assignment_resampling){
  
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  assert_that(is.logical(equal_share_tr_assignment))
  
  if(is.null(data_src)){
   data_src<-"random_generated_data/random_data"
  }
  
  str<-sprintf("benchmark_%s_N%d_NTrees%d_eshF%d_eshS%d",
               data_src,N,n_trees,equal_share_tr_assignment*1,
               equal_share_tr_assignment_resampling*1)
  
  return (str)
}
