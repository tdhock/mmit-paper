source("packages.R")

## TODO: seems to hang on
##  19 /   25 lymphoma.mkatayama test fold=1

## TODO: cache! if(file.exists(predictions.csv)...

set.dir.vec <- Sys.glob(file.path("data", "*"))
penaltyLearning.predictions <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  features <- fread(file.path(set.dir, "features.csv"))
  targets <- fread(file.path(set.dir, "targets.csv"))
  folds <- fread(file.path(set.dir, "folds.csv"))
  feature.mat <- as.matrix(features)
  target.mat <- as.matrix(targets)
  fold.vec <- sort(unique(folds$fold))
  pred.dt <- data.table(pred.log.penalty=rep(NA_real_, nrow(features)))
  param.mat <- matrix(
    NA_real_, length(fold.vec), ncol(features)+1,
    dimnames=list(fold=fold.vec, feature=c("(Intercept)", colnames(features))))
  for(test.fold in fold.vec){
    cat(sprintf(
      "%4d / %4d %s test fold=%d\n",
      set.dir.i, length(set.dir.vec),
      set.name, test.fold))
    is.test <- folds$fold == test.fold
    is.train <- !is.test
    set.seed(1)
    fit <- IntervalRegressionCV(
      feature.mat[is.train,], target.mat[is.train,])
    pred.dt[is.test, pred.log.penalty := predict(fit, feature.mat[is.test, ])]
    param.mat[paste(test.fold), rownames(fit$pred.param.mat)] <-
      fit$pred.param.mat
  }
  out.dir <- file.path("predictions", "IntervalRegressionCV", set.name)
  dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
  fwrite(pred.dt, file.path(out.dir, "predictions.csv"))
  fwrite(data.table(param.mat), file.path(out.dir, "parameters.csv"))
  penaltyLearning.predictions[[set.name]] <- list(
    parameters=param.mat,
    predictions=pred.dt)
}

save(penaltyLearning.predictions, file="penaltyLearning.predictions.RData")
