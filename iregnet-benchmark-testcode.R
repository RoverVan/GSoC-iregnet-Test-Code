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
row.type <- "martix.row.length"
col.type <- "martix.col.length"

#Set the constant number for testing in row or col
constant.col.number <- 10
constant.row.number <- 5000

#Set the update number for testing in row or col
update.row.length <- seq(100, 10000, by=500)
update.col.length <- seq(100, 4000, by=1000)


##Main process

dists <- c("gaussian", "logistic")
censoring.types <- c("none", "right", "left", "interval")

for(dist in dists){
  
  dist <- "logistic";
  for(censoring.type in censoring.types){
    
    #This is for testing in row
    benchmark.main(row.type, update.row.length, dist, censoring.type)  
    
    #This is for testing in col
    benchmark.main(col.type, update.col.length, dist, censoring.type)
  }
}


#The main benchmark function of iregnet
benchmark.main<- function(type, update.length, dist, censoring.type){
  
  #Init the result list.
  opt.res <- list()
  
  for(i in update.length){
    
    #Set the change number in this for loop
    update.number = i
    
    if(type == "martix.row.length"){
      
      #In this part, we set the col as the constant.number, get the runtime data on the row
      opt.res[[paste(i, 2)]] <- benchmark.iregnet(update.number, update.number, constant.col.number, dist, censoring.type)
    }else {
      
      #In this part, we set the row as the constant.number, get the runtime data on the col
      opt.res[[paste(i, 2)]] <- benchmark.iregnet(update.number, constant.row.number, update.number, dist, censoring.type)
    }
  }
  benchmark.plot(opt.res, type, dist, censoring.type)
}

#Microbenchmark funtion to get the runtime data
benchmark.iregnet <- function(update.number, row.number, col.number, dist, censoring.type){
  
  #Get xy form iregnet test function get_xy()
  xy <- get_xy(as.numeric(row.number), as.numeric(col.number), censoring.type, standardize=F, T)
  
  #use microbenchmark to test runtime
  time.result <- microbenchmark(oldiregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                iregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=3L)
  
  #split the time result by expr
  res <- data.table(time.result)[, list(min=min(time), mean=mean(time), max=max(time)), by=list(expr)][order(expr)]
  
  sm.diff <- data.frame(rep(update.number,times=2), c(res$min[1], res$min[2]), c(res$mean[1], res$mean[2]), 
                        c(res$max[1], res$max[2]), c("old_iregnet", "new_iregnet"))
  
  ggplot(sm.diff)
  
  #combine as a new data frame and return
  return(sm.diff);
  
}

#Microbenchmark funtion to get the runtime data (with glmnet)
benchmark.iregnet.glmnet <- function(update.number, row.number, col.number, dist, censoring.type){
  
  #Get xy form iregnet test function get_xy()
  xy <- get_xy(as.numeric(row.number), as.numeric(col.number), censoring.type, standardize=F, T)
  
  #use microbenchmark to test runtime
  time.result <- microbenchmark(oldiregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                iregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                glmnet(xy$x, xy$y[,1], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=5L)
  
  #split the time result by expr
  res <- data.table(time.result)[, list(min=min(time), mean=mean(time), max=max(time)), by=list(expr)][order(expr)]
  
  sm.diff <- data.frame(rep(update.number,times=3), c(res$min[1], res$min[2], res$min[3]), c(res$mean[1], res$mean[2], res$mean[3]), 
                        c(res$max[1], res$max[2], res$max[3]), c("old_iregnet", "new_iregnet", "glmnet"))
  
  ggplot(sm.diff)
  
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
    guides(fill="none")+
    ylab("Runtime (ms)")
  
  ggsave(paste(dist, test.type, censoring.type, ".png"), path = "/Users/Rover/Desktop/", width = 9.35, height = 6.32)
}

