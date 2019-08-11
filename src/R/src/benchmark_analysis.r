
get_means_order<-function(b_data){
  assert_that(is.data.frame(b_data))
  
  means<-numeric(length(b_data[1,]))
  for(i in 1:length(means)){
    means[i]<-mean(b_data[,i])
  }
  order(means)
}

get_sv_success_rate<-function(b_data){
  assert_that(is.data.frame(b_data))
  
  successes<-0
  for(i in 1:length(b_data[,1])){
    if(b_data[[i,1]]==b_data[[i,2]]){
      successes <- successes+1
    }
  }
  success_rate<-round(successes/length(b_data[,1])*100,digits = 2)
  
  df<-data.frame("Runs"=length(b_data[,1]),
                 "Successes"=successes,
                 "Success rate"=paste(success_rate,"%",sep = ""),
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