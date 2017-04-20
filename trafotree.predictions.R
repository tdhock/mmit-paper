source("packages.R")
library(trtf)#"0.1.1")#R CMD INSTALL ctm/pkg/trtf/  svn up -r 698

trafotreeNormal <- function(X, y, ...){
  finite.targets <- data.frame(log.penalty=y[is.finite(y)])
  m <- ctm(as.basis(~log.penalty, data=finite.targets), todistr="Normal")
  train.Surv <- Surv(y[, 1], y[,2], type="interval2")
  train.df <- data.frame(log.penalty=train.Surv, X)
  mlt.fit <- mlt(m, data=train.df)
  trafotree(
    m, formula = log.penalty ~ ., data=train.df,
    mltargs=list(theta=coef(mlt.fit)),
    control=ctree_control(...))
}

trafotreePredict <- function(fit, X.new){
  df <- data.frame(X.new)
  cf <- predict(fit, df, type="coef")
  -cf[,"(Intercept)"] / cf[,"log.penalty"]
}
 
trafotreeCV <- function
(X.mat, y.mat, n.folds=4,
 mc.seq=c(
   seq(0, 0.85, by=0.05),
   seq(0.9, 1, by=0.01))
 ){
  fold.vec <- sample(rep(1:n.folds, l=nrow(y.mat)))
  tv.list <- list()
  for(validation.fold in unique(fold.vec)){
    is.validation <- fold.vec==validation.fold
    is.train <- !is.validation
    tv.list[[paste(validation.fold)]] <- foreach(
      mc=mc.seq, .combine=rbind)%dopar%{
    ##for(mc in mc.seq){
        cat(sprintf("vfold=%d mc=%f\n", validation.fold, mc))
        fit <- trafotreeNormal(
          X.mat[is.train,],
          y.mat[is.train,],
          mincriterion=mc)
        pred.log.penalty <- trafotreePredict(fit, X.mat)
        is.lo <- pred.log.penalty < y.mat[,1]
        is.hi <- y.mat[,2] < pred.log.penalty
        data.table(is.train, is.error=is.lo|is.hi)[, {
          errors <- sum(is.error)
          data.table(validation.fold, mc, errors, error.percent=errors/.N*100)
        }, by=list(set=ifelse(is.train, "train", "validation"))]
      }
  }
  tv <- do.call(rbind, tv.list)
  tv.stats <- tv[, list(
    mean=mean(error.percent),
    sd=sd(error.percent)
    ), by=list(mc, set)]
  best.validation <- tv.stats[set=="validation",][which.min(mean),]
  gg <- ggplot()+
    geom_line(aes(
      mc, error.percent, color=set, group=paste(validation.fold, set)),
              data=tv)+
    geom_ribbon(aes(
      mc, ymin=mean-sd, ymax=mean+sd, fill=set),
                alpha=0.5,
                data=tv.stats)+
    geom_line(aes(
      mc, mean, color=set),
              size=2,
              data=tv.stats)+
    geom_point(aes(
      mc, mean, color=set),
               shape=21,
               fill="white",
               data=best.validation)
  print(gg)
  trafotreeNormal(
    feature.mat, target.mat, mincriterion=best.validation$mc)
}

set.dir.vec <- Sys.glob(file.path("data", "*"))
trafotree.predictions <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  out.dir <- file.path("predictions", "trafotree", set.name)
  predictions.csv <- file.path(out.dir, "predictions.csv")
  if(file.exists(predictions.csv)){
    pred.dt <- fread(predictions.csv)
  }else{
    features <- fread(file.path(set.dir, "features.csv"))
    targets <- fread(file.path(set.dir, "targets.csv"))
    folds <- fread(file.path(set.dir, "folds.csv"))
    feature.mat <- as.matrix(features)
    target.mat <- as.matrix(targets)
    fold.vec <- sort(unique(folds$fold))
    pred.dt <- data.table(pred.log.penalty=rep(NA_real_, nrow(features)))
    for(test.fold in fold.vec){
      cat(sprintf(
        "%4d / %4d %s test fold=%d\n",
        set.dir.i, length(set.dir.vec),
        set.name, test.fold))
      is.test <- folds$fold == test.fold
      is.train <- !is.test
      set.seed(1)
      fit <- trafotreeCV(
        feature.mat[is.train,], target.mat[is.train,])
      pred.dt[is.test, pred.log.penalty := {
        trafotreePredict(fit, feature.mat[is.test, ])
      }]
    }
    dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
    fwrite(pred.dt, predictions.csv)
  }
  trafotree.predictions[[set.name]] <- list(
    predictions=pred.dt)
}

trafotree0.95.predictions <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  out.dir <- file.path("predictions", "trafotree0.95", set.name)
  predictions.csv <- file.path(out.dir, "predictions.csv")
  if(file.exists(predictions.csv)){
    pred.dt <- fread(predictions.csv)
  }else{
    features <- fread(file.path(set.dir, "features.csv"))
    targets <- fread(file.path(set.dir, "targets.csv"))
    folds <- fread(file.path(set.dir, "folds.csv"))
    feature.mat <- as.matrix(features)
    target.mat <- as.matrix(targets)
    fold.vec <- sort(unique(folds$fold))
    pred.dt <- data.table(pred.log.penalty=rep(NA_real_, nrow(features)))
    for(test.fold in fold.vec){
      cat(sprintf(
        "%4d / %4d %s test fold=%d\n",
        set.dir.i, length(set.dir.vec),
        set.name, test.fold))
      is.test <- folds$fold == test.fold
      is.train <- !is.test
      set.seed(1)
      fit <- trafotreeNormal(
        feature.mat[is.train,], target.mat[is.train,])
      pred.dt[is.test, pred.log.penalty := {
        trafotreePredict(fit, feature.mat[is.test, ])
      }]
    }
    dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
    fwrite(pred.dt, predictions.csv)
  }
  trafotree0.95.predictions[[set.name]] <- list(
    predictions=pred.dt)
}

save(trafotree.predictions, trafotree0.95.predictions,
     file="trafotree.predictions.RData")


