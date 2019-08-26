
get_means<-function(b_data){
  assert_that(is.data.frame(b_data))
  
  means<-numeric(length(b_data[1,]))
  for(i in 1:length(means)){
    means[i]<-mean(b_data[,i])
  }
  means
}

get_sv_success_rate<-function(b_data){
  assert_that(is.data.frame(b_data))
  
  succ<-numeric(length(names(b_data))-2)
  for(i in 1:length(b_data[,1])){
    
    methods_results<-sort(b_data[i,3:length(names(b_data))])
    
    for(j in 1:length(succ)){
      if(b_data[[i,2]]==methods_results[j]){
        succ[j] <- succ[j]+1
        break
      }
    }
  }
  cum_succ <- cumsum(succ)
  succ_rate<-round(cum_succ/length(b_data[,1])*100,digits = 2)
  
  df<-data.frame("Nth best method" = 1:length(succ),
                 "Times picked"=succ,
                 "Cumulated picks"=cum_succ,
                 "Cumulative success rate"=paste(succ_rate,"%",sep = ""),
                 check.names = FALSE)
  
  return (df)
}

get_counts<-function(b_data){
  assert_that(is.data.frame(b_data))
  
  counts<-matrix(data = 0,nrow = 2,ncol = length(b_data[1,])-2)
  for(i in 1:length(b_data$oracle)){
    for (j in 3:length(b_data[1,])) {
      if(b_data$oracle[i]==b_data[i,j]){
        counts[1,j-2] <- counts[1,j-2]+1
      }
      if(b_data$synth_validation[i]==b_data[i,j]){
        counts[2,j-2] <- counts[2,j-2]+1
      }
    }
  }
  counts[1,] <- counts[1,]/sum(counts[1,]) 
  counts[2,] <- counts[2,]/sum(counts[2,]) 
  return (counts)
}