source("packages.R")

predictions.csv.vec <- Sys.glob(file.path(
  "predictions", "*", "*", "predictions.csv"))

test.error.list <- list()
test.auc.list <- list()
for(predictions.csv.i in seq_along(predictions.csv.vec)){
  pred.data.dir <- dirname(predictions.csv)
  set.name <- basename(pred.data.dir)
  pred.model.dir <- dirname(pred.data.dir)
  model.name <- basename(pred.model.dir)
  predictions.csv <- predictions.csv.vec[[predictions.csv.i]]
  predictions.dt <- fread(predictions.csv)
  targets.dt <- fread(file.path("data", set.name, "targets.csv"))
  folds.dt <- fread(file.path("data", set.name, "folds.csv"))
  targets.pred.fold <- data.table(targets.dt, predictions.dt, folds.dt)
  ## TODO: ROC curves from just this target interval dt.
  ## ROCtargets(targets.pred.fold) or ..?
  test.error.list[[paste(set.name, model.name)]] <- targets.pred.fold[, {
    fp <- sum(pred.log.penalty < min.log.penalty)
    fn <- sum(max.log.penalty < pred.log.penalty)
    list(
      set.name, model.name,
      fp, fn, errors=fp+fn, labels=.N,
      possible.fp=sum(is.finite(min.log.penalty)),
      possible.fn=sum(is.finite(max.log.penalty)))
  }, by=fold]
}
