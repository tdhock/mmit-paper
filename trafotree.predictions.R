source("packages.R")

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
 
n.folds <- 4
fold.vec <- sample(rep(1:n.folds, l=nrow(target.mat)))
for(validation.fold in unique(fold.vec)){
  is.validation <- fold.vec==validation.fold
  is.train <- !is.validation
  for(mc in seq(0.8, 1, by=0.05)){
    fit <- trafotreeNormal(
      feature.mat[is.train,],
      target.mat[is.train,],
      mincriterion=mc)
    pred.vec <- predict(fit, data.frame(feature.mat))
    pred.log.penalty <- coef(fit)[paste(pred.vec), "log.penalty"]
    is.lo <- pred.log.penalty < target.mat[,1]
    is.hi <- target.mat[,2] < pred.log.penalty
    ##TODO: how to ??
    data.table(target.mat, is.train, is.lo, is.hi, is.error=is.lo|is.hi)[, {
      errors <- sum(is.error)
      data.table(errors, error.percent=errors/.N*100)
    }, by=list(set=ifelse(is.train, "train", "validation"))]
  }
}

set.dir.vec <- Sys.glob(file.path("data", "*"))
trafotree.predictions <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  out.dir <- file.path("predictions", "trafotree", set.name)
  predictions.csv <- file.path(out.dir, "predictions.csv")
  parameters.csv <- file.path(out.dir, "parameters.csv")
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
      pred.dt[is.test, pred.log.penalty := predict(fit, feature.mat[is.test, ])]
    }
    dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
    fwrite(pred.dt, predictions.csv)
  }
  trafotree.predictions[[set.name]] <- list(
    parameters=param.mat,
    predictions=pred.dt)
}

save(trafotree.predictions, file="trafotree.predictions.RData")


