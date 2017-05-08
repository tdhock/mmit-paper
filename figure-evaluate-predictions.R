source("packages.R")

load("evaluate.predictions.RData")
load("data.sets.RData")

##dput(RColorBrewer::brewer.pal(Inf, "Set2"))
algo.colors <- c(
  "#66C2A5",
  trafotree="#FC8D62",
  trafotree0.95="#8DA0CB",
  mmit.linear.hinge="#E78AC3",
  mmit.squared.hinge="#A6D854", "#FFD92F", 
  IntervalRegressionCV="#E5C494",
  constant="#B3B3B3")#grey

data.set.sizes <- data.table(set.name=names(data.sets))[, {
  s <- data.sets[[set.name]]
  list(
    observations=nrow(s$features),
    features=ncol(s$features),
    upper.limits=sum(is.finite(s$targets[, 2])),
    lower.limits=sum(is.finite(s$targets[, 1])),
    model=ifelse(
      grepl("joint", set.name), "Poisson joint", ifelse(
        grepl("FPOP", set.name), "Poisson FPOP", ifelse(
          grepl("PDPA", set.name), "Poisson PDPA", "Normal unconstrained"))))
}, by=set.name]
data.set.sizes[, percent.upper := 100*upper.limits/(upper.limits+lower.limits)]

evaluate.predictions[, accuracy.percent := 100 - error.percent]
min.nonzero <- evaluate.predictions[mean.squared.error!=0, min(mean.squared.error)]
evaluate.predictions[, log10.mean.squared.error := log10(mean.squared.error+min.nonzero)]
evaluate.tall <- melt(
  evaluate.predictions[model.name!="iregnet"],
  id.vars=c("fold", "set.name", "model.name"),
  measure.vars=c("auc", "accuracy.percent", "log10.mean.squared.error"))
mean.dt <- evaluate.tall[, list(
  mean=mean(value),
  sd=sd(value)
  ), by=list(set.name, model.name, variable)]
difficulty <- mean.dt[variable=="auc", {
  list(
    max=.SD[model.name != "constant", max(mean)],
    constant=.SD[model.name=="constant", mean]
    )}, by=list(set.name)]
difficulty[, improvement := max - constant]
set.levels <- difficulty[order(improvement), set.name]
set.levels <- data.set.sizes[order(observations), set.name]
setkey(data.set.sizes, set.name)
data.set.ord <- data.set.sizes[set.levels]
data.set.ord[, set.size := {
  sprintf("%s %4d=n %3d=p %2d%%up",
    set.name,
    observations,
    features,
    as.integer(percent.upper))
}]
data.set.ord[, set.fac := factor(set.size, set.size)]
setkey(data.set.ord, set.name)
setfac <- function(set.name){
  data.set.ord[set.name, set.fac]
}
mean.dt[, set.fac := setfac(set.name)]
evaluate.tall[, set.fac := setfac(set.name)]

gg.panels <- ggplot()+
  geom_point(aes(
    value, model.name),
             data=evaluate.tall)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set.name ~ variable, scales="free")

"H3K36me3_TDH_other_FPOP" #trafotree does a little better.

stats.wide <- dcast(
  evaluate.predictions,
  set.name + fold ~ model.name,
  value.var=c("auc", "accuracy.percent", "mean.squared.error"))

diff.pvalues <- stats.wide[, {
  L <- t.test(auc_mmit.squared.hinge, auc_IntervalRegressionCV, paired=TRUE)
  with(L, data.table(
    p.value,
    estimate
  ))
}, by=set.name][order(p.value),]
stats.wide[, set.fac := factor(set.name, diff.pvalues$set.name)]
break.vec <- seq(0.6, 1, by=0.2)
gg.scatter.auc <- ggplot()+
  ggtitle("MMIT with squared hinge loss does not improve AUC with respect to regularized linear model")+
  theme_bw()+
  facet_wrap("set.fac", ncol=8)+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_abline(slope=1,intercept=0,color="grey")+
  coord_equal()+
  geom_point(aes(
    auc_IntervalRegressionCV, auc_mmit.squared.hinge),
    shape=1,
    data=stats.wide)+
  scale_x_continuous(
    breaks=break.vec,
    labels=paste(break.vec))
print(gg.scatter.auc)

auc.wide <- dcast(
  evaluate.predictions,
  set.name + fold ~ model.name,
  value.var="auc")
auc.tall <- melt(
  auc.wide,
  id.vars=c("set.name", "fold", "IntervalRegressionCV"),
  measure.vars=c("mmit.linear.hinge", "mmit.squared.hinge", "trafotree", "constant"),
  variable.name="competitor.name",
  value.name="competitor.auc")
auc.tall[, set.fac := factor(set.name, diff.pvalues$set.name)]
gg.scatter.auc <- ggplot()+
  ggtitle("MMIT does not improve AUC with respect to regularized linear model")+
  theme_bw()+
  facet_wrap("set.fac", ncol=8)+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_abline(slope=1,intercept=0,color="grey")+
  coord_equal()+
  geom_point(aes(
    IntervalRegressionCV, competitor.auc, color=competitor.name),
    shape=1,
    data=auc.tall)+
  ##scale_color_manual(values=algo.colors)+
  scale_x_continuous(
    "Test AUC for regularized linear model",
    breaks=break.vec,
    labels=paste(break.vec))+
  scale_y_continuous(
    "Test AUC for MMIT competitor model")
print(gg.scatter.auc)

gg.scatter <- ggplot()+
  theme_bw()+
  facet_wrap("set.fac", ncol=8)+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_abline(slope=1,intercept=0)+
  coord_equal()+
  geom_point(aes(
    accuracy.percent_IntervalRegressionCV, accuracy.percent_mmit.squared.hinge),
    shape=1,
    data=stats.wide)
print(gg.scatter)

gg.scatter <- ggplot()+
  theme_bw()+
  facet_wrap("set.fac", ncol=8)+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_abline(slope=1,intercept=0)+
  coord_equal()+
  geom_point(aes(
    log10(mean.squared.error_IntervalRegressionCV),
    log10(mean.squared.error_mmit.squared.hinge)),
    shape=1,
    data=stats.wide)
print(gg.scatter)

gg.colors <- ggplot()+
  scale_color_manual(values=algo.colors)+
  geom_point(aes(
    mean, set.fac, color=model.name),
             alpha=0.5,
             size=5,
             data=mean.dt)+
  geom_point(aes(
    value, set.fac, color=model.name),
             shape=1,
             data=evaluate.tall)+
  theme_bw()+
  theme(
    legend.position="bottom",
    panel.margin=grid::unit(0, "lines"),
    axis.text.y=element_text(family="mono", size=10))+
  facet_grid(. ~ variable, scales="free")+
  xlab("Test accuracy, 5 folds and mean")+
  ylab("data set")

evaluate.tall[, type := "folds"]
gg.colors <- ggplot()+
  scale_fill_manual(values=c(mean=NA))+
  scale_color_manual(values=algo.colors)+
  geom_point(aes(
    mean, set.fac, color=model.name, key=paste(set.name, model.name)),
             alpha=0.5,
             size=5,
             data=mean.dt)+
  geom_segment(aes(
    mean-sd, set.fac,
    xend=mean+sd, yend=set.fac,
    color=model.name,
    key=paste(model.name, set.name)),
    alpha=0.5,
    data=mean.dt)+
  geom_point(aes(
    value, set.fac, color=model.name, fill=type),
    shape=1,
    data=evaluate.tall)+
  theme_bw()+
  theme(
    legend.position="bottom",
    panel.margin=grid::unit(0, "lines"),
    axis.text.y=element_text(family="mono", size=10))+
  facet_grid(. ~ variable, scales="free")+
  xlab("Test accuracy, 5 folds and mean")+
  ylab("data set")

viz <- list(
  first=list(
    type=c(),
    model.name=c("constant", "mmit.squared.hinge")),
  dots=gg.colors+
    theme_animint(width=1000))
animint2dir(viz, "figure-evaluate-predictions")

png("figure-evaluate-predictions.png", 16, 9, units="in", res=100)
print(gg.scatter.auc)
dev.off()
