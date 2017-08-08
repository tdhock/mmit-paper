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

test.fold <- 1
is.test <- fold.vec==test.fold
is.train <- !is.test
train.targets.mat <- targets.mat[is.train,]
train.features.mat <- features.mat[is.train,]
tr <- trafotreeIntercept(train.features.mat, train.targets.mat)

## Torsten's plotting code.
nid <- min(nodeids(tr, terminal = TRUE))
pdf("figure-trafotree-bug-fold1.pdf", width = 15, height = 12)
plot(rotate(tr), terminal_panel = trtf:::node_mlt, nobs.loc='top',
     tp_args = list(type = "distribution", id = FALSE, ylines = 3, K = 100, 
       fill = "lightblue", xaxis = nid))
dev.off()

devtools::session_info()
