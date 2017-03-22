source("packages.R")

data(neuroblastomaProcessed, package="penaltyLearning")

with(neuroblastomaProcessed, {
  fwrite(data.table(feature.mat), "neuroblastomaProcessed_features.csv")
  fwrite(data.table(target.mat), "neuroblastomaProcessed_targets.csv")
})

## To see selecting one variable or another, try n.row=50 with
## margin=0 or 1.

feature.name.vec <- c("log.mad", "log.n", "log.bases.per.probe", "log.rss.1")
margin <- 0.5
min.diff.feature <- 1e-10
log.n <- neuroblastomaProcessed$feature.mat[, "log.mad"]
i.vec <- which(-2.515 < log.n & log.n < -2.5096)
n.row <- nrow(neuroblastomaProcessed$feature.mat)
n.row <- 100
i.vec <- as.integer(seq(1, nrow(neuroblastomaProcessed$feature.mat), l=n.row))
target.dt.list <- list()
thresh.dt.list <- list()
slack.dt.list <- list()
for(feature.name in feature.name.vec){
  feature.vec <- neuroblastomaProcessed$feature.mat[i.vec, feature.name]
  ord <- order(feature.vec)
  feature.ord.vec <- feature.vec[ord]
  target.ord.mat <- neuroblastomaProcessed$target.mat[i.vec, ][ord, ]
  result.fwd <- compute_optimal_costs(target.ord.mat, margin)
  if(FALSE){
    ## manually compute cost to verify. O(n^2) matrix rows=data, cols=thresh.
    slack.mat.list <- sapply(1:nrow(result.fwd), function(i){
      some.targets <- target.ord.mat[1:i, , drop=FALSE]
      sign.mat <- matrix(c(-1, 1), i, 2, byrow=TRUE)
      ## slack = [s_i (pred - y_i) + margin]_+
      pred <- result.fwd$pred[i]
      slack.mat <- sign.mat*(pred-some.targets)+margin
      slack.mat[slack.mat < 0 | slack.mat==Inf] <- 0
      slack.mat
    })
    slack.fwd <- sapply(slack.mat.list, sum)
    data.table(feature.ord.vec, slack.fwd, result.fwd)
    thresh[, .(feature, slack.below, cost)]
    thresh[, plot(log10(diff.feature), log10(slack.diff))]
  }
  target.rev.mat <- target.ord.mat[nrow(target.ord.mat):1, ]
  stopifnot(identical(rownames(target.ord.mat), rev(rownames(target.rev.mat))))
  result.rev <- compute_optimal_costs(target.rev.mat, margin)
  print(rbind(
    fwd=result.fwd[n.row,],
    rev=result.rev[n.row,]))
  both <- data.table(
    feature=feature.ord.vec[-length(feature.ord.vec)],
    diff.feature=diff(feature.ord.vec),
    result.fwd[1:(nrow(result.fwd)-1), ],
    rev=result.rev[(nrow(result.rev)-1):1, ])
  both[, total.cost := cost + rev.cost]
  both[, threshold := feature + diff.feature/2]
  thresh <- both[{
    min.diff.feature < diff.feature & #do not consider very close thresholds.
      is.finite(rev.pred) & is.finite(pred) #do not consider inf predictions.
   },]
  plot(total.cost ~ feature, both)
  abline(v=thresh[which.min(total.cost), threshold], col="red")
  finite.limits <- data.table(
    feature.name,
    pid.chr=rownames(target.ord.mat),
    limit=as.numeric(target.ord.mat),
    type=as.character(matrix(
      c("min", "max"), nrow(target.ord.mat), 2, byrow=TRUE)),
    feature=as.numeric(matrix(
      feature.ord.vec, nrow(target.ord.mat), 2, byrow=FALSE))
      )[is.finite(limit),]
  target.dt.list[[feature.name]] <- finite.limits
  ## want to compute residuals for every threshold and finite target,
  ## matrix with rows for target limits and columns for thresholds.
  left.mat <- matrix(
    thresh$pred, nrow(finite.limits), nrow(thresh), byrow=TRUE)
  right.mat <- matrix(
    thresh$rev.pred, nrow(finite.limits), nrow(thresh), byrow=TRUE)
  thresh.mat <- matrix(
    thresh$threshold, nrow(finite.limits), nrow(thresh), byrow=TRUE)
  limit.mat <- matrix(
    finite.limits$limit, nrow(finite.limits), nrow(thresh), byrow=FALSE)
  feature.mat <- matrix(
    finite.limits$feature, nrow(finite.limits), nrow(thresh), byrow=FALSE)
  sign.mat <- matrix(
    ifelse(finite.limits$type=="min", -1, 1),
    nrow(finite.limits), nrow(thresh), byrow=FALSE)
  is.below.thresh <- feature.mat <= thresh.mat
  pred.mat <- ifelse(is.below.thresh, left.mat, right.mat)
  ## slack = [s_i (pred - y_i) + margin]_+
  slack.mat <- sign.mat * (pred.mat - limit.mat) + margin
  slack.mat[slack.mat < 0] <- 0
  slack.below.mat <- slack.mat
  slack.below.mat[!is.below.thresh] <- 0
  slack.above.mat <- slack.mat
  slack.above.mat[is.below.thresh] <- 0
  thresh[, slack.below := colSums(slack.below.mat)]
  thresh[, slack.above := colSums(slack.above.mat)]
  thresh[, slack := slack.below + slack.above]
  thresh[, slack.diff := total.cost - slack]
  thresh[, diff.above := rev.cost - slack.above]
  thresh[, diff.below := cost - slack.below]
  thresh[, thresh.i := seq_along(slack)]
  thresh[order(slack.diff),]
  plot(slack.diff ~ threshold, thresh)
  stopifnot(thresh[, all.equal(slack, total.cost)])
  is.saved <- 0 < slack.mat
  is.saved <- TRUE # for smooth transitions.
  if(any(is.saved)){
    slack.dt.list[[feature.name]] <- data.table(
      feature.name,
      threshold=thresh.mat[is.saved],
      sign=sign.mat[is.saved],
      slack=slack.mat[is.saved],
      pred=pred.mat[is.saved],
      feature=feature.mat[is.saved],
      limit=limit.mat[is.saved],
      row.i=row(limit.mat)[is.saved])
  }
  thresh.dt.list[[feature.name]] <- data.table(feature.name, thresh)
}
target.dt <- do.call(rbind, target.dt.list)
thresh.dt <- do.call(rbind, thresh.dt.list)
slack.dt <- do.call(rbind, slack.dt.list)

## if there are several thresholds with minimum slack, pick the
## threshold in the middle (this only happens when there are few data
## points, e.g. n.row=50.
min.thresh <- thresh.dt[total.cost==min(total.cost),]
min.thresh[, stopifnot(
  length(unique(feature.name))==1,
  pred==pred[1],
  rev.pred==rev.pred[1])]
min.feature <- min.thresh[1, feature]
max.feature <- min.thresh[.N, feature+diff.feature]
mid.feature <- (min.feature + max.feature)/2
best.thresh <- min.thresh[1,]
best.thresh[, pred.thresh := mid.feature]
show.thresh <- best.thresh
## Make a matrix margin or pred -1 0 1 on rows, left or right on
## columns.
sign.mat <- matrix(c(1, 0, -1), 3, 2, byrow=FALSE)
feature.range.mat <- apply(
  neuroblastomaProcessed$feature.mat, 2, range)
feature.range <- feature.range.mat[, show.thresh$feature.name]
model.dt <- data.table(
  show.thresh,
  feature.min=as.numeric(matrix(
    c(feature.range[1], show.thresh$pred.thresh), 3, 2, byrow=TRUE)),
  feature.max=as.numeric(matrix(
    c(show.thresh$pred.thresh, feature.range[2]), 3, 2, byrow=TRUE)),
  log.penalty=as.numeric(
    show.thresh[, matrix(c(pred, rev.pred), 3, 2, byrow=TRUE)]+
    margin*sign.mat),
  line=ifelse(as.numeric(sign.mat)==0, "prediction", "margin"))

## The matrices below have 6 columns: left lower margin, right lower
## margin, left pred, right pred, left upper margin, right upper
## margin.
all.pred.mat <- thresh.dt[, matrix(rep(c(pred, rev.pred), 3), .N, 6)]
all.margin.mat <- matrix(
  margin*rep(c(-1, 0, 1), each=2), nrow(thresh.dt), 6, byrow=TRUE)
all.model.dt <- data.table(
  thresh.dt,
  feature.min=thresh.dt[, as.numeric(matrix(
    rep(c(feature.range.mat[1, feature.name], threshold), 3),
    .N, 6))],
  feature.max=thresh.dt[, as.numeric(matrix(
    rep(c(threshold, feature.range.mat[2, feature.name]), 3),
    .N, 6))],
  line.side=rep(c("left", "right"), each=nrow(thresh.dt)),
  line.sign=as.numeric(sign(all.margin.mat)),
  line=ifelse(as.numeric(all.margin.mat)==0, "prediction", "margin"),
  log.penalty=as.numeric(all.pred.mat+all.margin.mat))
    
best.slack <- slack.dt[show.thresh, on=.(feature.name, threshold), nomatch=0L]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(y ~ feature.name, scales="free")+
  geom_step(aes(
    feature, total.cost, color=cost.computation),
    size=2,
    data=data.table(thresh.dt, y="total.cost", cost.computation="C++"))+
  geom_step(aes(
    feature, slack, color=cost.computation),
    size=1,
    data=data.table(thresh.dt, y="total.cost", cost.computation="R"))+
  geom_step(aes(
    feature, cost, color=cost.computation),
    size=2,
    data=data.table(thresh.dt, y="below.cost", cost.computation="C++"))+
  geom_step(aes(
    feature, slack.below, color=cost.computation),
    size=1,
    data=data.table(thresh.dt, y="below.cost", cost.computation="R"))+
  geom_step(aes(
    feature, rev.cost, color=cost.computation),
    size=2,
    data=data.table(thresh.dt, y="above.cost", cost.computation="C++"))+
  geom_step(aes(
    feature, slack.above, color=cost.computation),
    size=1,
    data=data.table(thresh.dt, y="above.cost", cost.computation="R"))+
  geom_point(aes(
    pred.thresh, total.cost),
    data=data.table(show.thresh, y="total.cost"),
    shape=1,
    color="violet")+
  geom_point(aes(
    feature, limit, fill=type),
    shape=21,
    data=data.table(target.dt, y="log(penalty)"))+
  scale_fill_manual(values=c(min="blue", max="white"))+
  ylab("")+
  geom_segment(aes(
    feature, limit,
    xend=feature, yend=limit+sign*slack),
    data=data.table(best.slack, y="log(penalty)"),
    color="red")+
  geom_segment(aes(
    feature.min, log.penalty,
    linetype=line,
    xend=feature.max, yend=log.penalty),
    data=data.table(model.dt, y="log(penalty)"),
    size=1,
    color="red")+
  geom_vline(aes(
    xintercept=pred.thresh),
    data=show.thresh,
    linetype="dotted",
    color="violet")+
  scale_linetype_manual(values=c(prediction="solid", margin="dotted"))

ggplot()+
  geom_step(aes(
    feature, total.cost,
    group=feature.name),
    data=data.table(thresh.dt, y="total.cost"),
    size=2)

viz <- list(
  features=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1000)+
    facet_grid(y ~ feature.name, scales="free")+
    geom_step(aes(
      feature, total.cost),
      data=data.table(thresh.dt, y="total.cost"),
      size=2)+
    geom_tallrect(aes(
      xmin=feature, xmax=feature+diff.feature,
      clickSelects.variable=paste0(feature.name, ".thresh"),
      clickSelects.value=threshold),
      data=data.table(thresh.dt, y="total.cost"),
      alpha=0.2,
      size=2)+
    geom_point(aes(
      threshold, total.cost,
      key=feature.name,
      showSelected.variable=paste0(feature.name, ".thresh"),
      showSelected.value=threshold),
      data=data.table(thresh.dt, y="total.cost"),
      fill=NA,
      color="violet")+
    geom_vline(aes(
      xintercept=threshold,
      key=feature.name,
      showSelected.variable=paste0(feature.name, ".thresh"),
      showSelected.value=threshold),
      data=thresh.dt,
      linetype="dotted",
      color="violet")+
    scale_fill_manual(values=c(min="blue", max="white"))+
    ylab("")+
    geom_segment(aes(
      feature, limit,
      showSelected.variable=paste0(feature.name, ".thresh"),
      showSelected.value=threshold,
      key=paste(feature.name, row.i),
      xend=feature, yend=limit+sign*slack),
      data=data.table(slack.dt, y="log(penalty)"),
      color="red")+
    geom_segment(aes(
      feature.min, log.penalty,
      linetype=line,
      showSelected.variable=paste0(feature.name, ".thresh"),
      showSelected.value=threshold,
      key=paste(feature.name, line.side, line.sign),
      xend=feature.max, yend=log.penalty),
      data=data.table(all.model.dt, y="log(penalty)"),
      size=1,
      color="red")+
    scale_linetype_manual(values=c(prediction="solid", margin="dotted"))+
    geom_point(aes(
      feature, limit,
      key=pid.chr,
      fill=type),
      data=data.table(target.dt, y="log(penalty)")),
  duration=list(),
  first=list())
viz$duration[paste0(unique(thresh.dt$feature.name), ".thresh")] <- 2000
best.each <- thresh.dt[, .SD[which.min(total.cost),], by=feature.name]
viz$first[paste0(best.each$feature.name, ".thresh")] <- best.each$threshold
animint2dir(viz, "figure-penaltyLearning")

gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(y ~ feature.name, scales="free")+
    geom_segment(aes(
      feature, total.cost,
      xend=feature+diff.feature, yend=total.cost),
      data=data.table(thresh.dt, y="total.cost"),
      size=2)+
    geom_point(aes(
      pred.thresh, total.cost),
      data=data.table(show.thresh, y="total.cost"),
      shape=1,
      color="violet")+
    geom_point(aes(
      feature, limit, fill=type),
      shape=21,
      data=data.table(target.dt, y="log(penalty)"))+
    scale_fill_manual(values=c(min="blue", max="white"))+
    ylab("")+
    geom_segment(aes(
      feature, limit,
      xend=feature, yend=limit+sign*slack),
      data=data.table(best.slack, y="log(penalty)"),
      color="red")+
    geom_segment(aes(
      feature.min, log.penalty,
      linetype=line,
      xend=feature.max, yend=log.penalty),
      data=data.table(model.dt, y="log(penalty)"),
      size=1,
      color="red")+
    geom_vline(aes(
      xintercept=pred.thresh),
      data=show.thresh,
      linetype="dotted",
      color="violet")+
  scale_linetype_manual(values=c(prediction="solid", margin="dotted"))
png("figure-penaltyLearning.png", 6, 6, units="in", res=100)
print(gg)
dev.off()

## TODO: hold out half data, compute test error for different margin
## parameters.
