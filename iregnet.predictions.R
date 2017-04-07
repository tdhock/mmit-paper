source("packages.R")

set.dir.vec <- Sys.glob(file.path("data", "*"))
iregnet.predictions <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  out.dir <- file.path("predictions", "iregnet", set.name)
  predictions.csv <- file.path(out.dir, "predictions.csv")
  parameters.csv <- file.path(out.dir, "parameters.csv")
  if(file.exists(predictions.csv)){
    pred.dt <- fread(predictions.csv)
    param.dt <- fread(parameters.csv)
    param.mat <- as.matrix(param.dt)
  }else{
    features <- fread(file.path(set.dir, "features.csv"))
    targets <- fread(file.path(set.dir, "targets.csv"))
    folds <- fread(file.path(set.dir, "folds.csv"))
    feature.mat <- as.matrix(features)
    target.mat <- as.matrix(targets)
    fold.vec <- sort(unique(folds$fold))
    pred.dt <- data.table(pred.log.penalty=rep(NA_real_, nrow(features)))
    param.mat <- matrix(
      NA_real_, length(fold.vec), ncol(features)+1,
      dimnames=list(
        fold=fold.vec,
        feature=c("(Intercept)", colnames(features))))
    for(test.fold in fold.vec){
      cat(sprintf(
        "%4d / %4d %s test fold=%d\n",
        set.dir.i, length(set.dir.vec),
        set.name, test.fold))
      is.test <- folds$fold == test.fold
      is.train <- !is.test
      set.seed(1)
      fit <- cv.iregnet(
        feature.mat[is.train,], target.mat[is.train,],
        family="gaussian")
      pred.dt[is.test, pred.log.penalty := predict(fit, feature.mat[is.test, ])]
      param.mat[paste(test.fold), rownames(fit$pred.param.mat)] <-
        fit$pred.param.mat
    }
    dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
    fwrite(pred.dt, predictions.csv)
    fwrite(data.table(param.mat), parameters.csv)
  }
  iregnet.predictions[[set.name]] <- list(
    parameters=param.mat,
    predictions=pred.dt)
}

save(iregnet.predictions, file="iregnet.predictions.RData")
