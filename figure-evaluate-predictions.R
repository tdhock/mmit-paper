source("packages.R")

load("evaluate.predictions.RData")
load("data.sets.RData")

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
evaluate.tall <- melt(
  evaluate.predictions[model.name!="iregnet"],
  id.vars=c("fold", "set.name", "model.name"),
  measure.vars=c("auc", "accuracy.percent"))
mean.dt <- evaluate.tall[, list(
  mean=mean(value)
  ), by=list(set.name, model.name, variable)]
difficulty <- mean.dt[variable=="auc", {
  list(
    max=.SD[model.name != "constant", max(mean)],
    constant=.SD[model.name=="constant", mean]
    )}, by=list(set.name)]
difficulty[, improvement := max - constant]
set.levels <- difficulty[order(improvement), set.name]
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

##dput(RColorBrewer::brewer.pal(Inf, "Set2"))
algo.colors <- c(
  IntervalRegressionCV="#66C2A5",
  trafotree="#FC8D62",
  trafotree0.95="#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", 
  "#E5C494",
  constant="#B3B3B3")#grey
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

png("figure-evaluate-predictions.png", 10, 7, units="in", res=100)
print(gg.colors)
dev.off()
