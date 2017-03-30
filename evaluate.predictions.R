source("packages.R")

predictions.csv.vec <- Sys.glob(file.path(
  "predictions", "*", "*", "predictions.csv"))
evaluate.predictions.list <- list()
for(predictions.csv.i in seq_along(predictions.csv.vec)){
  predictions.csv <- predictions.csv.vec[[predictions.csv.i]]
  pred.data.dir <- dirname(predictions.csv)
  set.name <- basename(pred.data.dir)
  pred.model.dir <- dirname(pred.data.dir)
  model.name <- basename(pred.model.dir)
  predictions.dt <- fread(predictions.csv)
  targets.dt <- fread(file.path("data", set.name, "targets.csv"))
  folds.dt <- fread(file.path("data", set.name, "folds.csv"))
  targets.pred.fold <- data.table(targets.dt, predictions.dt, folds.dt)
  if(13 < nrow(targets.dt)){
    evaluate.predictions.list[[predictions.csv]] <- targets.pred.fold[, {
      roc.list <- targetIntervalROC(
        cbind(min.log.penalty, max.log.penalty), pred.log.penalty)
      with(roc.list, data.table(
        set.name,
        model.name,
        auc, error.percent=thresholds[threshold=="predicted", error.percent]))
    }, by=list(fold)]
  }
}
evaluate.predictions <- do.call(rbind, evaluate.predictions.list)

save(evaluate.predictions, file="evaluate.predictions.RData")
