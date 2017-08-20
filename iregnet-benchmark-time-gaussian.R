#Before the Test, we loaded the necessary library
library(oldiregnet)
library(iregnet)
library(glmnet)
library(microbenchmark)
library(ggplot2)
library(data.table)


#In this Test, we mainly test guasssian distribution. 
dist = "gaussian";

#We set the seed into 100
seed_test = 100
set.seed(seed_test)

#Set the constant number for testing in row or col
constant.col.number <- 10
constant.row.number <- 100

################################ Gaussian - None #####################################

#Init the result list.
#This is for testing in row
opt.res.row <- list() 

#This is for testing in col
opt.res.col <- list() 

for(i in 1:10){
  
  #Set the change number in this for loop
  numberobs = i * 100
  
  #In this part, we set the col as the constant.number, get the runtime data on the row
  xy <- get_xy(as.numeric(numberobs), constant.col.number, "none", standardize=F, T)
  time.result <- microbenchmark(oldiregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                iregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                glmnet(xy$x, xy$y[,1], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=5L)
  
  res <- data.table(time.result)[, list(min=min(time), mean=mean(time), max=max(time)), by=list(expr)][order(expr)]
  
  opt.res.row[[paste(i, 3)]] <- data.frame(c(numberobs, numberobs, numberobs), c(res$min[1], res$min[2], res$min[3]), c(res$mean[1], res$mean[2], res$mean[3]), 
                                       c(res$max[1], res$max[2], res$max[3]), c("old_iregnet", "new_iregnet", "glmnet"))
  
  #In this part, we set the row as the constant.number, get the runtime data on the col
  xy <- get_xy(constant.row.number, as.numeric(numberobs), "none", standardize=F, T)
  time.result <- microbenchmark(oldiregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                iregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                glmnet(xy$x, xy$y[,1], dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=5L)
  
  res <- data.table(time.result)[, list(min=min(time), mean=mean(time), max=max(time)), by=list(expr)][order(expr)]
  
  opt.res.col[[paste(i, 3)]] <- data.frame(c(numberobs, numberobs, numberobs), c(res$min[1], res$min[2], res$min[3]), c(res$mean[1], res$mean[2], res$mean[3]), 
                                       c(res$max[1], res$max[2], res$max[3]), c("old_iregnet", "new_iregnet", "glmnet"))
  
}
res.row.none <- do.call(rbind, opt.res.row)

varname <- c("martix.row.length", "min", "mean", "max", "method");
colnames(res.row.none) <- varname
p <- ggplot(res.row.none, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in gaussian (none-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p

res.col.none <- do.call(rbind, opt.res.row)

varname <- c("martix.col.length", "min", "mean", "max", "method");
colnames(res.col.none) <- varname
p <- ggplot(res.col.none, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in gaussian (none-consering) against Matrix Col Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p

######################################################################################
################################ Gaussian - Right ####################################
######################################################################################
opt.before.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "right", standardize=F, T)
  time.result <- microbenchmark(iregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=5L)
  minvar <- min(time.result$time) 
  meanvar <- mean(time.result$time)
  maxvar <- max(time.result$time)
  
  opt.before.res <- rbind(opt.before.res, c(numberobs, minvar, meanvar, maxvar))
}

varname <- c("martix.row.length", "min", "mean", "max");
colnames(opt.before.res) <- varname
opt.before.res <- cbind(opt.before.res, method=c("new_iregnet"))
t.g.right.new <- opt.before.res

opt.after.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "right", standardize=F, T)
  time.result <- microbenchmark(oldiregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=5L)
  minvar <- min(time.result$time) 
  meanvar <- mean(time.result$time)
  maxvar <- max(time.result$time)
  
  opt.after.res <- rbind(opt.after.res, c(numberobs, minvar, meanvar, maxvar))
}

varname <- c("martix.row.length", "min", "mean", "max");
colnames(opt.after.res) <- varname
opt.after.res <- cbind(opt.after.res, method=c("old_iregnet"))
t.g.right.old <- opt.after.res

r.g.right <- rbind(t.g.right.new, t.g.right.old)
p <- ggplot(r.g.right, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in gaussian (right-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p

######################################################################################
################################ Gaussian - Left ####################################
######################################################################################
opt.before.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "left", standardize=F, T)
  time.result <- microbenchmark(iregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=5L)
  minvar <- min(time.result$time) 
  meanvar <- mean(time.result$time)
  maxvar <- max(time.result$time)
  
  opt.before.res <- rbind(opt.before.res, c(numberobs, minvar, meanvar, maxvar))
}

varname <- c("martix.row.length", "min", "mean", "max");
colnames(opt.before.res) <- varname
opt.before.res <- cbind(opt.before.res, method=c("new_iregnet"))
t.g.left.new <- opt.before.res

opt.after.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "left", standardize=F, T)
  time.result <- microbenchmark(oldiregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=5L)
  minvar <- min(time.result$time) 
  meanvar <- mean(time.result$time)
  maxvar <- max(time.result$time)
  
  opt.after.res <- rbind(opt.after.res, c(numberobs, minvar, meanvar, maxvar))
}

varname <- c("martix.row.length", "min", "mean", "max");
colnames(opt.after.res) <- varname
opt.after.res <- cbind(opt.after.res, method=c("old_iregnet"))
t.g.left.old <- opt.after.res

r.g.left <- rbind(t.g.left.new, t.g.left.old)
p <- ggplot(r.g.left, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in gaussian (left-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p

######################################################################################
################################ Gaussian - Interval #################################
######################################################################################
opt.before.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "interval", standardize=F, T)
  time.result <- microbenchmark(iregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=3L)
  minvar <- min(time.result$time) 
  meanvar <- mean(time.result$time)
  maxvar <- max(time.result$time)
  
  opt.before.res <- rbind(opt.before.res, c(numberobs, minvar, meanvar, maxvar))
}

varname <- c("martix.row.length", "min", "mean", "max");
colnames(opt.before.res) <- varname
opt.before.res <- cbind(opt.before.res, method=c("new_iregnet"))
t.g.interval.new <- opt.before.res

opt.after.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "interval", standardize=F, T)
  time.result <- microbenchmark(oldiregnet(xy$x, xy$surv, dist, alpha = 1, intercept = T, thresh=1e-7, standardize=T),
                                times=3L)
  minvar <- min(time.result$time) 
  meanvar <- mean(time.result$time)
  maxvar <- max(time.result$time)
  
  opt.after.res <- rbind(opt.after.res, c(numberobs, minvar, meanvar, maxvar))
}

varname <- c("martix.row.length", "min", "mean", "max");
colnames(opt.after.res) <- varname
opt.after.res <- cbind(opt.after.res, method=c("old_iregnet"))
t.g.interval.old <- opt.after.res

r.g.interval <- rbind(t.g.interval.new, t.g.interval.old)
p <- ggplot(r.g.interval, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in gaussian (interval-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p
