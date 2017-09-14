source("packages.R")

fun.list <- list(
  constant01=function(mat){
    roc <- targetIntervalROC(mat, rep(0, nrow(mat)))
    min.row <- roc$thresholds[threshold=="min.error"]
    min.row[, {
      if(min.thresh == -Inf){
        max.thresh-1
      }else if(max.thresh == Inf){
        min.thresh+1
      }else{
        (min.thresh+max.thresh)/2
      }
    }]#Inf sometimes!
  },
  constantMSE=function(mat){
    cost.df <- mmit::compute_optimal_costs(mat, 0, "square")
    cost.df[nrow(cost.df), "pred"]
  },
  constant=function(mat){
    limit.vec <- sort(unique(mat[is.finite(mat)]))
    thresh.vec <- limit.vec[-1]-diff(limit.vec)
    ## Create a matrix [observations x pred]
    pred.mat <- matrix(
      thresh.vec, nrow(mat), length(thresh.vec), byrow=TRUE)
    errors.vec <- colSums(
      pred.mat < mat[,1] | mat[,2] < pred.mat)
    pred <- thresh.vec[which.min(errors.vec)]
    if(interactive()){
      plot(errors.vec ~ thresh.vec)
      abline(v=pred)
    }
    pred
  })
    
set.dir.vec <- Sys.glob(file.path("data", "*"))
constant.predictions.list <- list()
for(set.dir.i in seq_along(set.dir.vec)){
  set.dir <- set.dir.vec[[set.dir.i]]
  set.name <- basename(set.dir)
  for(fun.name in names(fun.list)){
    fun <- fun.list[[fun.name]]
    out.dir <- file.path("predictions", fun.name, set.name)
    predictions.csv <- file.path(out.dir, "predictions.csv")
    parameters.csv <- file.path(out.dir, "parameters.csv")
    folds <- fread(file.path(set.dir, "folds.csv"))
    if(file.exists(predictions.csv)){
      pred.dt <- fread(predictions.csv)
    }else{
      targets <- fread(paste(
        "sed 's/inf/Inf/'",
        shQuote(file.path(set.dir, "targets.csv"))))
      target.mat <- as.matrix(targets)
      fold.vec <- sort(unique(folds$fold))
      pred.dt <- data.table(pred.log.penalty=rep(NA_real_, nrow(targets)))
      for(test.fold in fold.vec){
        cat(sprintf(
          "%4d / %4d %s test fold=%d\n",
          set.dir.i, length(set.dir.vec),
          set.name, test.fold))
        is.test <- folds$fold == test.fold
        is.train <- !is.test
        train.mat <- target.mat[is.train, ]
        pred <- fun(train.mat)
        pred.dt[is.test, pred.log.penalty := pred]
      }#test.fold
      dir.create(out.dir, showWarnings=TRUE, recursive=TRUE)
      fwrite(pred.dt, predictions.csv)
    }#file exists
    constant.predictions.list[[paste(
      set.name, fun.name)]] <- data.table(
        set.name, fun.name, unique(data.table(pred.dt, folds)))
  }#fun.name
}
constant.predictions <- do.call(rbind, constant.predictions.list)

save(constant.predictions, file="constant.predictions.RData")
