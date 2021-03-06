#Before the Test, we loaded the necessary library
library(oldiregnet)
library(iregnet)
library(glmnet)
library(microbenchmark)
library(ggplot2)
library(data.table)

#We set the seed into 100
seed_test = 100
set.seed(seed_test)

#Set the rowname of the benchmark table
row.type <- "matrix.row.length"
col.type <- "matrix.col.length"

#Set the constant number for testing in row or col
constant.col.number <- 10
constant.row.number <- 1000

#Set the largest row & col number for get_xy()
largest.col.size <- 10000
largest.row.size <- 100000

#Set the update number for testing in row or col
update.col.length <- as.integer(10^seq(2, 3, l=5))
update.row.length <- as.integer(10^seq(2, 4, l=5))

##Main process
dists <- c("gaussian", "logistic")
censoring.types <- c("none", "right", "left", "interval")

for(dist in dists){
  
  for(censoring.type in censoring.types){
    
    #This is for testing in row
    benchmark.main(row.type, update.row.length, dist, censoring.type)  
    
    #This is for testing in col
    benchmark.main(col.type, update.col.length, dist, censoring.type)
  }
}

#The main benchmark function of iregnet
benchmark.main<- function(type, update.length, dist, censoring.type){
  

  #Get xy form iregnet test function get_xy()
  simulated.data <- get_xy(largest.row.size, largest.col.size, censoring.type, standardize=F, T)
  
  #Init the result list.
  opt.res <- list()
  
  for(i in update.length){
    
    #Set the change number in this for loop
    update.number = i
    
    if(type == "matrix.row.length"){
      
      #In this part, we set the col as the constant.number, get the runtime data on the row
      opt.res[[paste(i, 2)]] <- benchmark.iregnet(simulated.data, update.number, update.number, constant.col.number, dist, censoring.type)
    }else {

      #In this part, we set the row as the constant.number, get the runtime data on the col
      opt.res[[paste(i, 2)]] <- benchmark.iregnet(simulated.data, update.number, constant.row.number, update.number, dist, censoring.type)
    }
  }
  benchmark.plot(opt.res, type, dist, censoring.type)
}

#Microbenchmark funtion to get the runtime data
benchmark.iregnet <- function(xy, update.number, row.number, col.number, dist, censoring.type){
  
  #use microbenchmark to test runtime
  time.result <- microbenchmark(oldiregnet(xy$x[1:row.number, 1:col.number], xy$surv[1:row.number], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                iregnet(xy$x[1:row.number, 1:col.number], xy$surv[1:row.number], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=3L)
  
  #split the time result by expr
  res <- data.table(time.result)[, list(min=min(time), mean=mean(time), max=max(time)), by=list(expr)][order(expr)]
  
  sm.diff <- data.frame(rep(update.number,times=2), c(res$min[1], res$min[2]), c(res$mean[1], res$mean[2]), 
                        c(res$max[1], res$max[2]), c("old_iregnet", "new_iregnet"))
  
  #combine as a new data frame and return
  return(sm.diff);
}

#Microbenchmark funtion to get the runtime data (with glmnet)
benchmark.iregnet.glmnet <- function(xy, update.number, row.number, col.number, dist, censoring.type){
  
  #use microbenchmark to test runtime
  time.result <- microbenchmark(oldiregnet(xy$x[1:row.number, 1:col.number], xy$surv[1:row.number], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                iregnet(xy$x[1:row.number, 1:col.number], xy$surv[1:row.number], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                glmnet(xy$x[1:row.number, 1:col.number], xy$y[1:row.number,1], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=3L)
  
  #split the time result by expr
  res <- data.table(time.result)[, list(min=min(time), mean=mean(time), max=max(time)), by=list(expr)][order(expr)]
  
  sm.diff <- data.frame(rep(update.number,times=3), c(res$min[1], res$min[2], res$min[3]), c(res$mean[1], res$mean[2], res$mean[3]), 
                        c(res$max[1], res$max[2], res$max[3]), c("old_iregnet", "new_iregnet", "glmnet"))
  
  #combine as a new data frame and return
  return(sm.diff);
}

#ggplot funtion to plot the benchmark
benchmark.plot <- function(opt.res, test.type, dist, censoring.type){
  
  opt.res <- do.call(rbind, opt.res)
  varname <- c(test.type, "min", "mean", "max", "method");
  colnames(opt.res) <- varname
  
  p <- ggplot(opt.res, aes_string(test.type))+
    geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
    geom_line(aes(y=mean/1e6, group=method, colour=method))+
    ggtitle(paste("Runtime in", dist, "(",censoring.type , "- consering) against", test.type))+
    guides(fill="none")+scale_y_log10()+scale_x_log10()+
    ylab("Runtime (ms)")
  ggsave(paste(dist, test.type, censoring.type, ".png"), path = "/Users/Rover/Desktop/", width = 9.35, height = 6.32)
}

