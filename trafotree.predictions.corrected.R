source("packages.R")

library(trtf)#"0.1.1")#R CMD INSTALL ctm/pkg/trtf/  svn up -r 698
## more recent: svn up -r 734 && R CMD INSTALL basefun mlt trtf

## I set mc.cores in ~/.Rprofile
if(is.null(getOption("mc.cores"))){
  options(mc.cores=2)
}

## This is the even newer version of trafotree which only splits on
## intercept/mean, and includes a post-processing step after model
## fitting, that checks for leaves with all of one censoring type. For
## any such leaves we provide a corrected predicted value.
trafotreeCorrected <- function(X, y, ...){
  df <- data.frame(
    log.penalty = Surv(y[,1], y[,2], type = "interval2"),
    X)
  yb <- as.basis(~log.penalty, data=data.frame(log.penalty = as.double(5:30)),
                 ui = matrix(c(0, 1), nrow = 1), ci = 0)
  m <- ctm(yb, todistr="Normal")
  ## takes ages because the Turnbull estimator in survival
  ## is slow when computing starting values
  mlt.fit <- mlt(m, data=df)
  tree.fit <- trafotree(
    m, formula = log.penalty ~ ., data = df, parm = 1,
    mltargs = list(theta = coef(mlt.fit)), stump = FALSE,
    control=ctree_control(...))
  by.result <- by(is.finite(y), predict(tree.fit), colSums)
  finite.count.mat <- do.call(rbind, by.result)
  cf <- coef(tree.fit)
  pred.vec <- -cf[,"(Intercept)"] / cf[,"log.penalty"]
  ## The following line is necessary because R drops the names if
  ## there is only one node in the tree.
  names(pred.vec) <- rownames(cf)
  largest.lower <- by(y[,1], predict(tree.fit), max)
  smallest.upper <- by(y[,2], predict(tree.fit), min)
  no.upper <- finite.count.mat[,2]==0
  no.lower <- finite.count.mat[,1]==0
  pred.vec[no.upper] <- largest.lower[no.upper]
  pred.vec[no.lower] <- smallest.upper[no.lower]
  tree.fit$pred.log.lambda <- pred.vec
  list(
    fit=tree.fit,
    pred.vec=pred.vec)
}

trafotreePredict <- function(L, X.new){
  df <- data.frame(X.new)
  node.vec <- predict(L$fit, df)
  L$pred.vec[paste(node.vec)]
}

trafotreeCV <- function
(X.mat, y.mat, n.folds=4,
 mc.seq=c(
   seq(0, 0.85, by=0.05),
   seq(0.9, 1, by=0.01))
 ){
  fold.vec <- sample(rep(1:n.folds, l=nrow(y.mat)))
  tv.list <- lapply(unique(fold.vec), function(validation.fold){
    is.validation <- fold.vec==validation.fold
    is.train <- !is.validation
    one.fold.list <- parallel::mclapply(mc.seq, function(mc){
      cat(sprintf("vfold=%d mc=%f\n", validation.fold, mc))
      fit <- trafotreeCorrected(
        X.mat[is.train,],
        y.mat[is.train,],
        mincriterion=mc)
      pred.log.penalty <- trafotreePredict(fit, X.mat)
      res.vec <- targetIntervalResidual(y.mat, pred.log.penalty)
      data.table(is.train, residual=res.vec)[, {
        errors <- sum(residual!=0)
        data.table(
          validation.fold, mc, errors,
          mse=mean(residual * residual),
          error.percent=errors/.N*100)
      }, by=list(set=ifelse(is.train, "train", "validation"))]
    })
    do.call(rbind, one.fold.list)
  })
  tv <- do.call(rbind, tv.list)
  tv[, log10.mse := log10(mse)]
  tv.tall <- melt(
    tv, measure.vars=c("log10.mse", "error.percent"),
    id.vars=c("set", "validation.fold", "mc"))
  tv.stats <- tv.tall[, list(
    mean=mean(value),
    sd=sd(value),
    median=median(value),
    q25=quantile(value, 0.25),
    q75=quantile(value, 0.75)
    ), by=list(mc, set, variable)]
  best.validation <-
    tv.stats[variable=="log10.mse" & set=="validation"][which.min(median)]
  gg <- ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(variable ~ ., scales="free")+
    geom_line(aes(
      mc, value, color=set, group=paste(validation.fold, set)),
              data=tv.tall)+
    geom_ribbon(aes(
      mc, ymin=q25, ymax=q75, fill=set),
                alpha=0.5,
                data=tv.stats)+
    geom_line(aes(
      mc, median, color=set),
              size=2,
              data=tv.stats)+
    ## geom_ribbon(aes(
    ##   mc, ymin=mean-sd, ymax=mean+sd, fill=set),
    ##             alpha=0.5,
    ##             data=tv.stats)+
    ## geom_line(aes(
    ##   mc, mean, color=set),
    ##           size=2,
    ##           data=tv.stats)+
    geom_point(aes(
      mc, median, color=set),
               shape=21,
               fill="white",
               data=best.validation)
  print(gg)
  trafotreeCorrected(X.mat, y.mat, mincriterion=best.validation$mc)
}

fit.fun.list <- list(
  TTreeCorrected0.95=trafotreeCorrected,
  TTreeCorrected=trafotreeCV)
set.dir.vec <- Sys.glob(file.path("data", "*"))
trafotree.predictions.corrected <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  for(model.name in names(fit.fun.list)){
    fit.fun <- fit.fun.list[[model.name]]
    out.dir <- file.path("predictions", model.name, set.name)
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
        fit <- fit.fun(
          feature.mat[is.train,], target.mat[is.train,])
        pred.dt[is.test, pred.log.penalty := {
          trafotreePredict(fit, feature.mat[is.test, ])
        }]
      }
      dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
      fwrite(pred.dt, predictions.csv)
    }
    trafotree.predictions.corrected[[paste(
      set.name, model.name)]] <- data.table(
      set.name, model.name, pred.dt)
  }#for(model.name
}

save(trafotree.predictions.corrected,
     file="trafotree.predictions.corrected.RData")

