set.seed(29)
library("trtf")# svn up -r 734

set.name <- "H3K27ac-H3K4me3_TDHAM_BP_FPOP"
targets.dt <- read.csv(paste0(set.name, "_targets.csv"))
targets.mat <- as.matrix(targets.dt)
features.dt <- read.csv(paste0(set.name, "_features.csv"))
features.mat <- as.matrix(features.dt)

set.seed(1)
n.folds <- 2
fold.vec <- sample(rep(1:n.folds, l=nrow(targets.dt)))

### use whole dataset
y <- targets.mat
X <- features.mat
library("survival")
### you can work direclty with "penalty", no need to log() it first.
### use the log_basis() basis function (with ui = "increasing")!
df <- data.frame(log.penalty = Surv(y[,1], y[,2], type = "interval2"),
                 X)
### this is a trick, sets up a linear basis function
### YOU NEED positive slope (variance > 0)
yb <- as.basis(~log.penalty, data=data.frame(log.penalty = as.double(5:30)),
               ui = matrix(c(0, 1), nrow = 1), ci = 0)
m <- ctm(yb, todistr="Normal")
### takes ages because the Turnbull estimator in survival
### is slow when computing starting values
mlt.fit <- mlt(m, data=df)

### check against survreg: OK!
sr.fit <- survreg(log.penalty ~ 1, data = df, dist = "gaussian")
logLik(sr.fit)
logLik(mlt.fit)
c(coef(sr.fit), sr.fit$scale)
c(-coef(mlt.fit)[1], 1) / coef(mlt.fit)[2]

### stump first
tr <- trafotree(m, formula = log.penalty ~ ., data = df, 
                mltargs = list(theta = coef(mlt.fit)), stump = TRUE)

logLik(tr)
coef(tr)

df$node <- factor(predict(tr, newdata = df, type = "node"))

### check against survreg: OK!
sr.fit2 <- tapply(1:nrow(df), df$node, function(i) 
    survreg(log.penalty ~ 1, data = df[i,], dist = "gaussian"))
sum(sapply(sr.fit2, logLik))

### whole tree
tr <- trafotree(m, formula = log.penalty ~ ., data = df, 
                mltargs = list(theta = coef(mlt.fit)), stump = FALSE)

logLik(tr)
### some inverse sd's are almost zero, so these nodes
### have very large variance (ie, no information due to
### almost all obs being right-censored).
coef(tr)
df$node <- factor(predict(tr, newdata = df, type = "node"))

### check against survreg: OK!
sr.fit2 <- tapply(1:nrow(df), df$node, function(i) 
    survreg(log.penalty ~ 1, data = df[i,], dist = "gaussian"))
sum(sapply(sr.fit2, logLik))

### plot tree
library("ATR") ### from partykit R-forge
nid <- min(nodeids(tr, terminal = TRUE))
pdf("tree.pdf", width = 6, height = 10)
plot(rotate(tr), terminal_panel = trtf:::node_mlt, nobs.loc='top',
     tp_args = list(type = "distribution", id = FALSE, ylines = 3, K = 100, 
                    fill = "lightblue", xaxis = nid))

### partition wrt intercept only
tr <- trafotree(m, formula = log.penalty ~ ., data = df, parm = 1,
                mltargs = list(theta = coef(mlt.fit)), stump = FALSE)

logLik(tr)
### much nicer variance parameters now
coef(tr)

### plot tree; much better
nid <- min(nodeids(tr, terminal = TRUE))
plot(rotate(tr), terminal_panel = trtf:::node_mlt, nobs.loc='top',
     tp_args = list(type = "distribution", id = FALSE, ylines = 3, K = 100, 
                    fill = "lightblue", xaxis = nid))
dev.off()

