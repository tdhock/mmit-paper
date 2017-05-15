source("packages.R")

## This script aims to explore the accuracy of L1 regularized linear
## models, where the regularization and margin parameters are selected
## using different CV evaluation criteria (squared hinge loss, MSE,
## AUC, incorrect labels). Questiond: which of these criteria selects
## the model which yields most accurate predictions? Or are they all
## pretty much equivalent? And do these conclusions also hold for the
## tree model?

## Need to compute fit.list.RData first, copy from server (where it
## took a long time to compute).
if(!file.exists("fit.list.RData")){
  system("scp thocking@guillimin.hpc.mcgill.ca:fit.list.RData .")
}
if(!file.exists("wide.list.RData")){
  system("scp thocking@guillimin.hpc.mcgill.ca:wide.list.RData .")
}

## This is how we compute fit.list.RData on the server.
if(FALSE){
  library(future)
  plan(multiprocess)
  library(penaltyLearning)
  data(neuroblastomaProcessed)
  error.dt <- data.table(neuroblastomaProcessed$errors)
  error.dt[, pid.chr := paste0(profile.id, ".", chromosome)]
  setkey(error.dt, pid.chr)
  n.folds <- 5
  set.seed(1)
  fold.vec <- sample(rep(1:n.folds, l=nrow(neuroblastomaProcessed$target.mat)))
  ##fit.list <- list()
  load("fit.list.RData")
  for(test.fold in 1:n.folds){
    if(length(fit.list) < test.fold){
      is.test <- fold.vec == test.fold
      is.train <- !is.test
      train.target.mat <- neuroblastomaProcessed$target.mat[is.train, ]
      train.feature.mat <- neuroblastomaProcessed$feature.mat[is.train, ]
      print(test.fold)
      set.seed(1)
      fit.list[[test.fold]] <-  penaltyLearning::IntervalRegressionCV(
        train.feature.mat, train.target.mat,
        verbose=1,
        margin.vec=c(0, 10^c(-(5:2), seq(-1, 1, by=0.2))),
        incorrect.labels.db=error.dt)
    }
  }
  save(fit.list, fold.vec, file="fit.list.RData")
}

objs <- load("fit.list.RData")
fit.fold.vec <- fold.vec
objs <- load("wide.list.RData")
stopifnot(identical(fit.fold.vec, fold.vec))

set.list <- list(
  wide=wide.list,
  dense=fit.list)
vstats.list <- list()
for(set.name in names(set.list)){
  L <- set.list[[set.name]]
  for(test.fold in seq_along(fit.list)){
    fit <- L[[test.fold]]
    vstats.list[[paste(set.name, test.fold)]] <- data.table(
      set.name, test.fold, fit$plot.heatmap.tile)
  }
}
vstats <- do.call(rbind, vstats.list)
vstats.min <- vstats[, {
  .SD[mean==min(mean)]
}, by=list(variable, set.name, test.fold)]
mse.dt <- vstats[variable=="mean.squared.error"]
vstats.selected <- vstats.min[, {
  v <- .SD[[variable]]
  dt <- data.table(set.name, test.fold, .SD)
  mse <- mse.dt[dt, mean, on=list(set.name, test.fold, regularization, margin)]
  .SD[order(v, mse)][1]
}, by=list(variable, set.name, test.fold)]
selected.tab <- vstats.selected[, table(variable, test.fold, set.name)]
stopifnot(selected.tab==1)

data(neuroblastomaProcessed)

selected.fit.list <- list()
roc.curves.list <- list()
eval.metrics.list <- list()
for(model.i in 1:nrow(vstats.selected)){
  selected <- vstats.selected[model.i]
  is.test <- fold.vec == selected$test.fold
  is.train <- !is.test
  train.target.mat <- neuroblastomaProcessed$target.mat[is.train, ]
  train.feature.mat <- neuroblastomaProcessed$feature.mat[is.train, ]
  cat(sprintf("%4d / %4d\n", model.i, nrow(vstats.selected)))
  print(selected)
  selected.fit.list[[model.i]] <- fit <- selected[, {
    IntervalRegressionRegularized(
      train.feature.mat, train.target.mat,
      margin=margin,
      initial.regularization=regularization,
      factor.regularization=NULL)
  }]
  test.feature.mat <- neuroblastomaProcessed$feature.mat[is.test, ]
  pred.dt <- data.table(
    profile.id=sub("[.].*", "", rownames(test.feature.mat)),
    chromosome=sub(".*[.]", "", rownames(test.feature.mat)),
    pred.log.lambda=as.numeric(predict(fit, test.feature.mat)))
  roc.list <- ROChange(
    neuroblastomaProcessed$errors,
    pred.dt,
    problem.vars=c("profile.id", "chromosome"))
  roc.curves.list[[model.i]] <- data.table(selected, roc.list$roc)
  eval.metrics.list[[model.i]] <- with(roc.list, data.table(
    selected, auc, thresholds[threshold=="predicted"]))
}
roc.curves <- do.call(rbind, roc.curves.list)
eval.metrics <- do.call(rbind, eval.metrics.list)

save(vstats, vstats.min, vstats.selected,
     roc.curves, eval.metrics, selected.fit.list,
     file="margin.complexity.linear.RData")
