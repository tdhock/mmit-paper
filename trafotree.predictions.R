source("packages.R")

library(trtf)#"0.1.1")#R CMD INSTALL ctm/pkg/trtf/  svn up -r 698
## more recent: svn up -r 734 && R CMD INSTALL basefun mlt trtf

future::plan(multiprocess)

cores <- 2

## This is the new version of trafotree which only splits on
## intercept/mean (not slope/variance) -- better prediction accuracy.
trafotreeIntercept <- function(X, y, ...){
  . <- log.penalty <- NULL
  df <- data.frame(
    log.penalty = Surv(y[,1], y[,2], type = "interval2"),
    X)
  ## RED FLAG :: MAYBE SWITCH TO something based on the outputs in the
  ## training data (y).
  yb <- as.basis(~log.penalty, data=data.frame(log.penalty = as.double(5:30)),
                 ui = matrix(c(0, 1), nrow = 1), ci = 0)
  m <- ctm(yb, todistr="Normal")
  ## takes ages because the Turnbull estimator in survival
  ## is slow when computing starting values
  mlt.fit <- mlt(m, data=df)
  trafotree(
    m, formula = log.penalty ~ ., data = df, parm = 1,
    mltargs = list(theta = coef(mlt.fit)), stump = FALSE,
    control=ctree_control(...))
}

## This is the old version of trafotree which splits on slope/variance
## as well as intercept/mean -- bad prediction accuracy due to
## overfitting.
trafotreeNormal <- function(X, y, ...){
  . <- log.penalty <- NULL
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
   seq(0.9, 1, by=0.01)),
 fun=trafotreeIntercept
 ){
  fold.vec <- sample(rep(1:n.folds, l=nrow(y.mat)))
  tv.list <- lapply(unique(fold.vec), function(validation.fold){
    is.validation <- fold.vec==validation.fold
    is.train <- !is.validation
    f.list <- parallel::mclapply(mc.seq, function(mc){
      cat(sprintf("vfold=%d mc=%f\n", validation.fold, mc))
      fit <- fun(
        X.mat[is.train,],
        y.mat[is.train,],
        mincriterion=mc)
      pred.log.penalty <- trafotreePredict(fit, X.mat)
      is.lo <- pred.log.penalty < y.mat[,1]
      is.hi <- y.mat[,2] < pred.log.penalty
      is.error <- is.lo|is.hi
      data.table(is.train, is.error)[, {
        errors <- sum(is.error)
        data.table(validation.fold, mc, errors, error.percent=errors/.N*100)
      }, by=list(set=ifelse(is.train, "train", "validation"))]
    }, mc.cores=cores)
    do.call(rbind, f.list)
  })
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

# options(warn=2)
set.dir.vec <- Sys.glob(file.path("data", "*"))
TTreeIntOnly0.95.predictions <- parallel::mclapply(seq_along(set.dir.vec), function(set.dir.i){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  out.dir <- file.path("predictions", "TTreeIntOnly0.95", set.name)
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
      fit <- trafotreeIntercept(
        feature.mat[is.train,], target.mat[is.train,])
      pred.dt[is.test, pred.log.penalty := {
        trafotreePredict(fit, feature.mat[is.test, ])
      }]
    }
    dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
    fwrite(pred.dt, predictions.csv)
  }
  list(predictions=pred.dt)
}, mc.cores=cores)

TTreeIntOnly.predictions <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  out.dir <- file.path("predictions", "TTreeIntOnly", set.name)
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
  TTreeIntOnly.predictions[[set.name]] <- list(
    predictions=pred.dt)
}

save(TTreeIntOnly.predictions, TTreeIntOnly0.95.predictions,
     file="TTreeIntOnly.predictions.RData")
     file="TTreeIntOnly.predictions.RData")
