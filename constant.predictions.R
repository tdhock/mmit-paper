source("packages.R")

set.dir.vec <- Sys.glob(file.path("data", "*"))
constant.predictions <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  out.dir <- file.path("predictions", "constant", set.name)
  predictions.csv <- file.path(out.dir, "predictions.csv")
  parameters.csv <- file.path(out.dir, "parameters.csv")
  if(file.exists(predictions.csv)){
    pred.dt <- fread(predictions.csv)
    param.dt <- fread(parameters.csv)
    param.mat <- as.matrix(param.dt)
  }else{
    targets <- fread(file.path(set.dir, "targets.csv"))
    folds <- fread(file.path(set.dir, "folds.csv"))
    target.mat <- as.matrix(targets)
    fold.vec <- sort(unique(folds$fold))
    pred.dt <- data.table(pred.log.penalty=rep(NA_real_, nrow(targets)))
    param.mat <- matrix(
      NA_real_, length(fold.vec), 1,
      dimnames=list(
        fold=fold.vec,
        feature="(Intercept)"))
    for(test.fold in fold.vec){
      cat(sprintf(
        "%4d / %4d %s test fold=%d\n",
        set.dir.i, length(set.dir.vec),
        set.name, test.fold))
      is.test <- folds$fold == test.fold
      is.train <- !is.test
      train.mat <- target.mat[is.train, ]
      limit.vec <- sort(unique(train.mat[is.finite(train.mat)]))
      thresh.vec <- limit.vec[-1]-diff(limit.vec)
      ## Create a matrix [observations x pred]
      pred.mat <- matrix(
        thresh.vec, nrow(train.mat), length(thresh.vec), byrow=TRUE)
      errors.vec <- colSums(
        pred.mat < train.mat[,1] | train.mat[,2] < pred.mat)
      pred <- thresh.vec[which.min(errors.vec)]
      if(interactive()){
        plot(errors.vec ~ thresh.vec)
        abline(v=pred)
      }
      pred.dt[is.test, pred.log.penalty := pred]
      param.mat[paste(test.fold), ] <- pred
    }
    dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
    fwrite(pred.dt, predictions.csv)
    fwrite(data.table(param.mat), parameters.csv)
  }
  constant.predictions[[set.name]] <- list(
    parameters=param.mat,
    predictions=pred.dt)
}

save(constant.predictions, file="constant.predictions.RData")
