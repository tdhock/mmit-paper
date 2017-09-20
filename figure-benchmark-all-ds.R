source("packages.R")

load("evaluate.predictions.RData")
load("data.sets.RData")

dput(RColorBrewer::brewer.pal(Inf, "Paired"))
algo.colors <- c(
  "#66C2A5",
  TTreeIntOnly="#FC8D62",
  mmit.linear.hinge="#E78AC3",
  mmit.squared.hinge="#A6D854", "#FFD92F",
  IntervalRegressionCV="#E5C494",
  constant="#B3B3B3")
algo.colors <- c(
  TTreeIntOnly="#1F78B4",
  mmit.linear.hinge="#00ff00",
  mmit.squared.hinge="#ff3300",
  IntervalRegressionCV="#FDBF6F", constant="#ff66cc")

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
evaluate.predictions[, `-log10.mean.squared.error` := -log10(mean.squared.error+min.nonzero)]
evaluate.predictions[, `log10.mean.squared.error` := log10(mean.squared.error+min.nonzero)]
evaluate.tall <- melt(
  evaluate.predictions[model.name!="iregnet"],
  id.vars=c("fold", "set.name", "model.name"),
  measure.vars=c("log10.mean.squared.error"))
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
  sprintf("%s %4d=n %3d=p",
    set.name,
    observations,
    features)
}]
data.set.ord[, set.fac := factor(set.size, set.size)]
setkey(data.set.ord, set.name)
setfac <- function(set.name){
  data.set.ord[set.name, set.fac]
}
mean.dt[, set.fac := setfac(set.name)]
evaluate.tall[, set.fac := setfac(set.name)]


#gg.colors <- ggplot()+
#  scale_color_manual(values=algo.colors)+
#  geom_point(aes(
#    mean, set.fac, color=model.name),
#             alpha=0.5,
#             size=5,
#             data=mean.dt)+
#  geom_point(aes(
#    value, set.fac, color=model.name),
#             shape=1,
#             data=evaluate.tall)+
#  theme_bw()+
#  theme(
#    legend.position="bottom",
#    panel.margin=grid::unit(0, "lines"),
#    axis.text.y=element_text(family="mono", size=10))+
#  facet_grid(. ~ variable, scales="free")+
#  xlab("Test accuracy, 5 folds and mean")+
#  ylab("data set")

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
  xlab("Testing set log10(MSE) (mean and standard deviation)")+
  ylab("data set")

viz <- list(
  first=list(
    type=c(),
    model.name=c("constant", "mmit.squared.hinge", "mmit.linear.hinge", "IntervalRegressionCV", "TTreeIntOnly")),
  dots=gg.colors+
    theme_animint(height=1000, width=800))
animint2dir(viz, "figure-evaluate-predictions")

png("figure-evaluate-predictions.png", 16, 9, units="in", res=100)
dev.off()
