source("packages.R")

library(penaltyLearning)
library(trtf)
library(survival)

set.name <- "H3K27ac-H3K4me3_TDHAM_BP_FPOP"
for(pre in c("targets", "features")){
  from.csv <- paste0("data/", set.name, "/", pre, ".csv")
  to.csv <- paste0(set.name, "_", pre, ".csv")
  if(!file.exists(to.csv)){
    file.copy(from.csv, to.csv)
  }
}
targets.dt <- read.csv(paste0(set.name, "_targets.csv"))
targets.mat <- as.matrix(targets.dt)
features.dt <- read.csv(paste0(set.name, "_features.csv"))
features.mat <- as.matrix(features.dt)

set.seed(1)
n.folds <- 3
fold.vec <- sample(rep(1:n.folds, l=nrow(targets.dt)))

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

trafotreeIntercept <- function(X, y, ...){
  df <- data.frame(
    log.penalty = Surv(y[,1], y[,2], type = "interval2"),
    X)
  yb <- as.basis(~log.penalty, data=data.frame(log.penalty = as.double(5:30)),
                 ui = matrix(c(0, 1), nrow = 1), ci = 0)
  m <- ctm(yb, todistr="Normal")
  ## takes ages because the Turnbull estimator in survival
  ## is slow when computing starting values
  mlt.fit <- mlt(m, data=df)
  trafotree(
    m, formula = log.penalty ~ ., data = df, parm = 1,
    mltargs = list(theta = coef(mlt.fit)), stump = FALSE,
    control=ctree_control(...))
}

trafotreePredict <- function(fit, X.new){
  df <- data.frame(X.new)
  cf <- predict(fit, df, type="coef")
  -cf[,"(Intercept)"] / cf[,"log.penalty"]
}

trafotreeCV <- function
(X.mat, y.mat, n.folds=4,
 mc.seq=c(
   seq(0, 0.85, by=0.05),
   seq(0.9, 1, by=0.01)),
 fun=trafotreeIntercept
 ){
  fold.vec <- sample(rep(1:n.folds, l=nrow(y.mat)))
  tv.list <- lapply(unique(fold.vec), function(validation.fold){
    is.validation <- fold.vec==validation.fold
    is.train <- !is.validation
    f.list <- parallel::mclapply(mc.seq, function(mc){
      cat(sprintf("vfold=%d mc=%f\n", validation.fold, mc))
      fit <- fun(
        X.mat[is.train,],
        y.mat[is.train,],
        mincriterion=mc)
      pred.log.penalty <- trafotreePredict(fit, X.mat)
      is.lo <- pred.log.penalty < y.mat[,1]
      is.hi <- y.mat[,2] < pred.log.penalty
      is.error <- is.lo|is.hi
      data.table(is.train, is.error)[, {
        errors <- sum(is.error)
        data.table(validation.fold, mc, errors, error.percent=errors/.N*100)
      }, by=list(set=ifelse(is.train, "train", "validation"))]
    })
    do.call(rbind, f.list)
  })
  tv <- do.call(rbind, tv.list)
  tv.stats <- tv[, list(
    mean=mean(error.percent),
    sd=sd(error.percent)
    ), by=list(mc, set)]
  best.validation <- tv.stats[set=="validation",][which.min(mean),]
  gg <- ggplot()+
    geom_line(aes(
      mc, error.percent, color=set, group=paste(validation.fold, set)),
              data=tv)+
    geom_ribbon(aes(
      mc, ymin=mean-sd, ymax=mean+sd, fill=set),
                alpha=0.5,
                data=tv.stats)+
    geom_line(aes(
      mc, mean, color=set),
              size=2,
              data=tv.stats)+
    geom_point(aes(
      mc, mean, color=set),
               shape=21,
               fill="white",
               data=best.validation)
  print(gg)
  trafotreeNormal(
    X.mat, y.mat, mincriterion=best.validation$mc)
}

model.error.list <- list()
for(test.fold in 1:n.folds){
  print(test.fold)
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
  fit.cvtree <- trafotreeCV(train.features.mat, train.targets.mat)
  fit.int <- trafotreeIntercept(train.features.mat, train.targets.mat)
  test.features.mat <- features.mat[is.test,]
  pred.vec.list <- list(
    constant=rep(best.thresh, nrow(test.features.mat)),
    IntervalRegressionCV=predict(fit.linear, test.features.mat),
    TTreeIntOnly0.95=trafotreePredict(fit.int, test.features.mat),
    TTreeIntOnly=trafotreePredict(fit.cvtree, test.features.mat),
    trafotree0.95=trafotreePredict(fit.tree, test.features.mat))
  test.targets.mat <- targets.mat[is.test,]
  for(model.name in names(pred.vec.list)){
    print(model.name)
    pred.vec <- as.numeric(pred.vec.list[[model.name]])
    test.roc.list <- targetIntervalROC(test.targets.mat, pred.vec)
    res.vec <- targetIntervalResidual(test.targets.mat, pred.vec)
    test.metrics <- with(test.roc.list, {
      data.table(
        test.fold, model.name, auc, mse=sum(res.vec*res.vec),
        thresholds[threshold=="predicted"])
    })
    test.metrics[, accuracy.percent := 100-error.percent]
    test.metrics[, neg.log.mse := -log10(mse)]
    model.error.list[[paste(test.fold, model.name)]] <- test.metrics
  }
}
model.error <- do.call(rbind, model.error.list)

model.tall <- melt(
  model.error,
  measure.vars=c("auc", "neg.log.mse", "accuracy.percent"))
p <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ variable, scales="free")+
  geom_point(aes(value, model.name), data=model.tall)
print(p)
pdf("figure-trafotree-bug.pdf")
print(p)
dev.off()
