source("packages.R")

predictions.csv.vec <- Sys.glob(file.path(
  "predictions", "*", "*", "predictions.csv"))
evaluate.predictions.list <- list()
for(predictions.csv.i in seq_along(predictions.csv.vec)){
  predictions.csv <- predictions.csv.vec[[predictions.csv.i]]
  cat(sprintf(
    "%4d / %4d %s\n",
    predictions.csv.i, length(predictions.csv.vec), predictions.csv))
  pred.data.dir <- dirname(predictions.csv)
  set.name <- basename(pred.data.dir)
  pred.model.dir <- dirname(pred.data.dir)
  model.name <- basename(pred.model.dir)
  predictions.dt <- fread(predictions.csv)
  setnames(predictions.dt, "pred.log.penalty")
  targets.dt <- fread(file.path("data", set.name, "targets.csv"))
  folds.dt <- fread(file.path("data", set.name, "folds.csv"))
  stopifnot(nrow(predictions.dt) == nrow(targets.dt))
  targets.pred.fold <- data.table(targets.dt, predictions.dt, folds.dt)
  targets.pred.fold[, squared.error := ifelse(
    pred.log.penalty < min.log.penalty,
    (min.log.penalty-pred.log.penalty)^2, ifelse(
      max.log.penalty < pred.log.penalty,
      (max.log.penalty-pred.log.penalty)^2,
      0))]
  evaluate.predictions.list[[predictions.csv]] <- targets.pred.fold[, {
    roc.list <- targetIntervalROC(
      cbind(min.log.penalty, max.log.penalty), pred.log.penalty)
    with(roc.list, data.table(
      set.name,
      model.name,
      auc,
      error.percent=thresholds[threshold=="predicted", error.percent],
      mean.squared.error=mean(squared.error)))
  }, by=list(fold)]
}
evaluate.predictions <- do.call(rbind, evaluate.predictions.list)

save(evaluate.predictions, file="evaluate.predictions.RData")
