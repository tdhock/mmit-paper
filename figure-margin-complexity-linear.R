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
fit <- fit.list[[1]]
plot(fit)

for(vname in unique(fit$plot.heatmap.tile$variable)){
  cost.dt <- fit$plot.heatmap.tile[variable==vname]
  min.dt <- cost.dt[mean==min(mean)]
  if(vname=="negative.auc"){
    cost.dt[, mean := mean+1]
  }
  gg <- ggplot()+
    ggtitle(vname)+
    geom_point(aes(
      -log10(regularization),
      log10(margin),
      fill=log10(mean)),
      shape=21,
      size=3,
      color=NA,
      data=cost.dt)+
    geom_tile(aes(
      -log10(regularization),
      log10(margin),
      fill=log10(mean)),
      data=cost.dt)+
    scale_fill_gradient(low="white", high="red")+
    geom_point(aes(
      -log10(regularization),
      log10(margin)),
      data=min.dt,
      size=3,
      shape=1)
  pdf(paste0("figure-margin-complexity-", vname, ".pdf"))
  print(gg)
  dev.off()
}

data(neuroblastomaProcessed)
