.libPaths("library")
library(trtf)
library(survival)
library(ATR)

set.name <- "H3K27ac-H3K4me3_TDHAM_BP_FPOP"
targets.dt <- read.csv(paste0(set.name, "_targets.csv"))
targets.mat <- as.matrix(targets.dt)
features.dt <- read.csv(paste0(set.name, "_features.csv"))
features.mat <- as.matrix(features.dt)

set.seed(1)
n.folds <- 5
fold.vec <- sample(rep(1:n.folds, l=nrow(targets.dt)))

trafotreeCorrected <- function(X, y, ...){
  df <- data.frame(
    log.penalty = Surv(y[,1], y[,2], type = "interval2"),
    X)
  yb <- as.basis(~log.penalty, data=data.frame(log.penalty = as.double(5:30)),
                 ui = matrix(c(0, 1), nrow = 1), ci = 0)
  m <- ctm(yb, todistr="Normal")
  ## takes ages because the Turnbull estimator in survival
  ## is slow when computing starting values
  mlt.fit <- mlt(m, data=df)
  tree.fit <- trafotree(
    m, formula = log.penalty ~ ., data = df, parm = 1,
    mltargs = list(theta = coef(mlt.fit)), stump = FALSE,
    control=ctree_control(...))
  by.result <- by(is.finite(y), predict(tree.fit), colSums)
  finite.count.mat <- do.call(rbind, by.result)
  cf <- coef(tree.fit)
  pred.vec <- -cf[,"(Intercept)"] / cf[,"log.penalty"]
  largest.lower <- by(y[,1], predict(tree.fit), max)
  smallest.upper <- by(y[,2], predict(tree.fit), min)
  no.upper <- finite.count.mat[,2]==0
  no.lower <- finite.count.mat[,1]==0
  pred.vec[no.upper] <- largest.lower[no.upper]
  pred.vec[no.lower] <- smallest.upper[no.lower]
  tree.fit$pred.log.lambda <- pred.vec
  list(
    fit=tree.fit,
    pred.vec=pred.vec)
}

trafotreePredict <- function(L, X.new){
  df <- data.frame(X.new)
  node.vec <- predict(L$fit, df)
  L$pred.vec[paste(node.vec)]
}

test.fold <- 1
is.test <- fold.vec==test.fold
is.train <- !is.test
train.targets.mat <- targets.mat[is.train,]
train.features.mat <- features.mat[is.train,]
tr <- trafotreeCorrected(train.features.mat, train.targets.mat)

stopifnot(is.numeric(trafotreePredict(tr, features.mat[is.test,])))

## Torsten's plotting code.
nid <- min(nodeids(tr, terminal = TRUE))
pdf("figure-trafotree-bug-fold1.pdf", width = 15, height = 12)
plot(rotate(tr), terminal_panel = trtf:::node_mlt, nobs.loc='top',
     tp_args = list(type = "distribution", id = FALSE, ylines = 3, K = 100, 
       fill = "lightblue", xaxis = nid))
dev.off()

devtools::session_info()
