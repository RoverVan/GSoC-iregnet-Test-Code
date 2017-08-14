#Before Test
library(oldiregnet)
library(iregnet)
library(microbenchmark)
library(ggplot2)
dist = "logistic";

######################################################################################
################################ Logistic - None #####################################
######################################################################################
seed_test = 100
set.seed(seed_test)
opt.before.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "none", standardize=F, T)
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
t.l.none.new <- opt.before.res

opt.after.res <- data.frame();

for(i in 1:100){
  
  numberobs = i * 100
  xy <- get_xy(as.numeric(numberobs), 10, "none", standardize=F, T)
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
t.l.none.old <- opt.after.res

r.l.none <- rbind(t.l.none.new, t.l.none.old)
p <- ggplot(r.l.none, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in logistic (none-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p

######################################################################################
################################ Logistic - Right ####################################
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
t.l.right.new <- opt.before.res

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
t.l.right.old <- opt.after.res

r.l.right <- rbind(t.l.right.new, t.l.right.old)
p <- ggplot(r.l.right, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in logistic (right-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p

######################################################################################
################################ logistic - Left ####################################
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
t.l.left.new <- opt.before.res

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
t.l.left.old <- opt.after.res

r.l.left <- rbind(t.l.left.new, t.l.left.old)
p <- ggplot(r.l.left, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in logistic (left-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p

######################################################################################
################################ Logistic - Interval #################################
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
t.l.interval.new <- opt.before.res

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
t.l.interval.old <- opt.after.res

r.l.interval <- rbind(t.l.interval.new, t.l.interval.old)
p <- ggplot(r.l.interval, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in logistic (interval-consering) against Matrix Row Length")+
  guides(fill="none")+
  ylab("Runtime (ms)")
p