library(trtf)# svn up -r 734
library(penaltyLearning)

set.name <- "H3K27ac-H3K4me3_TDHAM_BP_FPOP"
targets.dt <- read.csv(paste0(set.name, "_targets.csv"))
targets.mat <- as.matrix(targets.dt)
features.dt <- read.csv(paste0(set.name, "_features.csv"))
features.mat <- as.matrix(features.dt)

set.seed(1)
n.folds <- 2
fold.vec <- sample(rep(1:n.folds, l=nrow(targets.dt)))

trafotreeNormal <- function(X, y, ...){
  finite.targets <- data.frame(log.penalty=y[is.finite(y)])
  m <- ctm(as.basis(~log.penalty, data=finite.targets), todistr="Normal")
  train.Surv <- survival::Surv(y[, 1], y[,2], type="interval2")
  train.df <- data.frame(log.penalty=train.Surv, X)
  mlt.fit <- mlt(m, data=train.df)
  trafotree(
    m, formula = log.penalty ~ ., data=train.df,
    mltargs=list(theta=coef(mlt.fit)),
    control=ctree_control(...))
}

trafotreePredict <- function(fit, X.new){
  df <- data.frame(X.new)
  cf <- predict(fit, df, type="coef")
  -cf[,"(Intercept)"] / cf[,"log.penalty"]
}

model.error.list <- list()
for(test.fold in 1:n.folds){
  is.test <- fold.vec==test.fold
  is.train <- !is.test
  train.targets.mat <- targets.mat[is.train,]
  train.features.mat <- features.mat[is.train,]
  train.roc.list <- targetIntervalROC(
    train.targets.mat, rep(0, nrow(train.targets.mat)))
  best.thresh <- train.roc.list$thresholds[
    threshold=="min.error", (min.thresh+max.thresh)/2]
  fit.linear <- IntervalRegressionCV(train.features.mat, train.targets.mat)
  fit.tree <- trafotreeNormal(train.features.mat, train.targets.mat)
  test.features.mat <- features.mat[is.test,]
  pred.vec.list <- list(
    constant=rep(best.thresh, nrow(test.features.mat)),
    IntervalRegressionCV=predict(fit.linear, test.features.mat),
    trafotree0.95=trafotreePredict(fit.tree, test.features.mat))
  test.targets.mat <- targets.mat[is.test,]
  for(model.name in names(pred.vec.list)){
    pred.vec <- as.numeric(pred.vec.list[[model.name]])
    test.roc.list <- targetIntervalROC(test.targets.mat, pred.vec)
    model.error.list[[paste(test.fold, model.name)]] <- with(test.roc.list, data.frame(
      test.fold, model.name, auc,
      thresholds[threshold=="predicted"]))
  }
}
model.error <- do.call(rbind, model.error.list)

p <- lattice::xyplot(model.name ~ error.percent, model.error)
print(p)
pdf("figure-trafotree-bug.pdf")
print(p)
dev.off()