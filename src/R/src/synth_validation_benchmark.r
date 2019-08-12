source("read_write_data.r")
source("synth_validation.r")
source("causal_inference_methods.r")
source("utilities.r")
source("plots.r")
source("benchmark_analysis.r")

benchmark<-function(data_src=NULL,N=100,n_trees=100,equal_share_tr_assignment=TRUE,
                    equal_share_tr_assignment_resampling=TRUE){
  
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  assert_that(is.logical(equal_share_tr_assignment_resampling))
  
  create_benchmark_directory()
  
  ate<-load_real_ate(data_src)
  
  data_row_c<-get_data_row_c(ate,data_src,N,n_trees,equal_share_tr_assignment,
                             equal_share_tr_assignment_resampling)
  print(data_row_c)
  
  save_entry_into_its_groups_and_regenerate_plots(data_src,N,n_trees,equal_share_tr_assignment,
                                                   equal_share_tr_assignment_resampling,
                                                   data_row_c)
}

save_entry_into_its_groups_and_regenerate_plots<-function(data_src,N,n_trees,equal_share_tr_assignment,
                                                           equal_share_tr_assignment_resampling,
                                                           data_row_c){
  
  b_data_src<-get_benchmark_data_group_names(data_src,N,n_trees,equal_share_tr_assignment,
                                             equal_share_tr_assignment_resampling)
  
  for(i in 1:length(b_data_src)){
    b_data<-add_row_to_file(data_row_c,b_data_src[i])
    update_errors_file(b_data,b_data_src[i])
    generate_plots(b_data,b_data_src[i]) 
  }
}

add_row_to_file<-function(data_row_c,b_data_src){
  
  assert_that(is.numeric(data_row_c))
  assert_that(is.character(b_data_src))
  
  data_available = tryCatch({
    df<-read_benchmark_data(b_data_src)
    TRUE
    
  },error = function(e) {
    FALSE
  })
  
  if(data_available){
    b_data<-read_benchmark_data(b_data_src)
    
    b_data<-rbind(b_data,as.list(data_row_c))
  }
  else{
    b_data<-data.frame(as.list(data_row_c))
    names(b_data)<-c("oracle","synth_validation",methods_names())
  }
  
  write_benchmark_data(b_data,b_data_src)
  
  b_data
}

update_errors_file<-function(b_data,b_data_src){
  b_data_src<-paste(b_data_src,"_mean_errors",sep = "")
  means<-get_means(b_data)
  mean_errors_data<-data.frame(as.list(means))
  names(mean_errors_data)<-names(b_data)
  write_benchmark_data(mean_errors_data,b_data_src)
}


generate_benchmark_data_plots<-function(data_src=NULL,N=100,n_trees=100,equal_share_tr_assignment=TRUE,
                                       equal_share_tr_assignment_resampling=TRUE){
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  assert_that(is.logical(equal_share_tr_assignment))
  
  b_data_src<-get_benchmark_data_group_names(data_src,N,n_trees,equal_share_tr_assignment,
                                     equal_share_tr_assignment_resampling)
  
  for(i in 1:length(b_data_src)){
    b_data<-read_benchmark_data(b_data_src[i])
    update_errors_file(b_data,b_data_src[i])
    generate_plots(b_data,b_data_src[i]) 
  }
}

get_benchmark_data_group_names<-function(data_src,N,n_trees,equal_share_tr_assignment,
                                         equal_share_tr_assignment_resampling){
  
  b_data_src<-get_benchmark_data_src(data_src,N,n_trees,equal_share_tr_assignment,
                                     equal_share_tr_assignment_resampling)
  
  b_data_src[2]<-ifelse(equal_share_tr_assignment,"equal_share_tr_assignment","population_share_tr_assignment")
  b_data_src[3]<-"all_runs"
  b_data_src[4]<-get_benchmark_data_type_src(data_src)
  
  return (b_data_src)
}

get_data_row_c<-function(ate,data_src,N,n_trees,equal_share_tr_assignment,
                       equal_share_tr_assignment_resampling){
  
  assert_that(is.numeric(ate))
  assert_that(is.null(data_src)||is.character(data_src))
  assert_integer(N)
  assert_that(is.logical(equal_share_tr_assignment))
  assert_that(is.logical(equal_share_tr_assignment_resampling))
  
  res<-synth_validation(data_src,N,n_trees,equal_share_tr_assignment,
                        equal_share_tr_assignment_resampling)
  
  avg_tr_errors<-abs(res$ates_methods-ate)
  
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

get_benchmark_data_type_src<-function(data_src){
  if(is.null(data_src)){
    data_src<-"random_generated_data/random_data"
  }
  
  #e.g random_generated_data
  data_type<-strsplit(data_src,"/",fixed = TRUE)[[1]][1]
  
  str<-sprintf("benchmark_%s/all_runs_%s",
               data_type,data_type)
  
  return (str)
}
