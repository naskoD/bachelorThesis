library(assertthat)
library(grid)
library(gridExtra)

source("benchmark_analysis.r")
source("causal_inference_methods.r")

my_barplot<-function(counts,add=FALSE){
  
  assert_that(is.matrix(counts))
  assert_that(is.logical(add))
  
  currentLoc<-Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "german")
  
  barplot(counts,
          col=c("lightsteelblue","gray80"),
          names.arg = names<-methods_names(),
          cex.main=1.5,
          xlab = "Methoden fur kausale Inferenz",
          ylab = "",
          main="Methodenauswahlh\344ufigkeit von Orakel und SV",
          beside=TRUE,
          las=1,
          axes = FALSE,
          cex.lab=1.2,
          add=add)
  
  legend("topleft", c("oracle","synth-validation"),
         fil=c("lightsteelblue","gray80"),
         cex=0.8, 
         horiz=FALSE,
         bg = "ivory2",
         inset = 0.02,
         box.lty=0)
  
    axis(2, at=pretty(counts), lab=paste0(pretty(counts) * 100, "%"), las=TRUE)
    
    title(ylab=enc2utf8("H\344ufigkeit (%)"), line=4, cex.lab=1.2)
    
    Sys.setlocale("LC_CTYPE", currentLoc)
}

my_boxplot<-function(b_data,add=FALSE,order_means){
  
  assert_that(is.data.frame(b_data))
  assert_that(is.logical(add))
  assert_that(is.numeric(order_means))
  
  currentLoc<-Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "german")
  
  boxplot(b_data[,order_means], 
          main = "MAE von den Methoden f\374r kausale Inferenz",
          cex.main=1.5,
          at = length(order_means):1,
          names = names(b_data)[order_means],
          las=1,
          col = "lightsteelblue",
          border = "black",
          horizontal = TRUE,
          outline = FALSE,
          add=add
  )
  
  Sys.setlocale("LC_CTYPE", currentLoc)
}
generate_success_rate_grid<-function(b_data,bench_data_src){
  assert_that(is.data.frame(b_data))
  assert_that(is.character(bench_data_src))
  
  currentLoc<-Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "german")
  
  success_rate_df<-get_sv_success_rate(b_data)
  
  jpeg(sprintf("../../../benchmark_data/%s.jpeg",paste0(bench_data_src,"_success_rate")),
       width = 550,
       height = 120)
  g<-tableGrob(success_rate_df,
               rows=NULL,
               theme = ttheme_default(base_size = 12,
                                      padding = unit(c(8,5),"mm")))
  grid.draw(g)
  dev.off()
  
  Sys.setlocale("LC_CTYPE", currentLoc)
}

generate_boxplot<-function(b_data,bench_data_src){
  assert_that(is.data.frame(b_data))
  assert_that(is.character(bench_data_src))
  
  means_order<-order(get_means(b_data))
  
  jpeg(sprintf("../../../benchmark_data/%s.jpeg",paste0(bench_data_src,"_avg_error_boxplot")),
       width = 650,
       height = 400)
  
  par(bg = "white")
  
  new_mar<-par("mar")
  new_mar[2]<-8
  par(mar=new_mar)
  
  my_boxplot(b_data,FALSE,means_order)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "ivory2")
  my_boxplot(b_data,TRUE,means_order)
  dev.off()
}

generate_barplot<-function(b_data,bench_data_src){
  assert_that(is.data.frame(b_data))
  assert_that(is.character(bench_data_src))
  
  pick_and_success_counts<-get_counts(b_data)
  
  jpeg(sprintf("../../../benchmark_data/%s.jpeg",paste0(bench_data_src,"_success_and_pick_rate_barplot")),
       width = 650,
       height = 400)
  
  par(bg = "white")
  
  new_mar<-par("mar")
  new_mar[2]<-6
  par(mar=new_mar)
  
  my_barplot(pick_and_success_counts,FALSE)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "ivory2")
  my_barplot(pick_and_success_counts,TRUE)
  dev.off()
  
}

generate_plots<-function(b_data,bench_data_src){
  assert_that(is.data.frame(b_data))
  assert_that(is.character(bench_data_src))
  
  generate_boxplot(b_data,bench_data_src)
  
  generate_success_rate_grid(b_data,bench_data_src)

  generate_barplot(b_data,bench_data_src)
}
