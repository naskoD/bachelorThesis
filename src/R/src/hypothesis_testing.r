source("read_write_data.r")
source("benchmark_analysis.r")


hypothesis_testing<-function(bench_data_src,significance){
  
  print(paste0("Dataset ",bench_data_src))
  
  b_data<-read_benchmark_data(bench_data_src)
  
  print("###########################################")
  print("###########################################")
  print(paste0("For a significance level of of ",significance ,", can we reject the null hypothesis that synth-validation isn't a better estimator than a specific method (the error of synth-validation is greater or equal than the error of this method)?"))
  
  for(i in 1:4){
    print("###########################################")
    print(paste0("method -> ",names(b_data)[2+i]))
    test<-t.test(b_data$synth_validation,b_data[,2+i],alternative = "less")
    print(test)
    res<-test$p.value 
    print(paste0("significance ->",significance))
    print(paste0("p-value ->",res))
    print(paste0("rejected? ->",res<significance))
  }
  
  print("###########################################")
  print("###########################################")
  print(paste0("For a significance level of ",significance ,", can we reject the null hypothesis that to use synth-validation isn't any better than picking from the methods at random (synth-validation doesn't pick one of the first n best estimations more often than by picking them at random)?"))
  
  
  cumulated_picks <- get_sv_success_rate(b_data)[,3]
  k <-cumulated_picks[4]
  
  for(i in 1:3){
    res<-pbinom(cumulated_picks[i],k,0.25*i,lower.tail = FALSE)
    print("###########################################")
    print(paste0("n = ",i))
    print(paste0("significance ->",significance))
    print(paste0("p-value ->",res))
    print(paste0("rejected? ->",res<significance))
  }
}

hypothesis_testing("all_runs",0.05)

hypothesis_testing("benchmark_real_data_raw/all_runs_real_data_raw",0.05)

hypothesis_testing("benchmark_real_data_simulated/all_runs_real_data_simulated",0.05)

hypothesis_testing("benchmark_random_generated_data/all_runs_random_generated_data",0.05)