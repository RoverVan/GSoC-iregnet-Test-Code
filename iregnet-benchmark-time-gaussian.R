#Before Test
library(oldiregnet)
library(iregnet)
library(microbenchmark)
library(ggplot2)
dist = "gaussian";

######################################################################################
################################ Gaussian - None #####################################
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
t.g.none.new <- opt.before.res

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
t.g.none.old <- opt.after.res

t.g.none <- rbind(t.g.none.new, t.g.none.old)
p <- ggplot(r1, aes(martix.row.length))+
  geom_ribbon(aes(ymin=min/1e6, ymax=max/1e6,fill=method, group=method), alpha=1/2)+
  geom_line(aes(y=mean/1e6, group=method, colour=method))+
  ggtitle("Runtime in gaussian (none-consering) against Matrix Row Length")+
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
