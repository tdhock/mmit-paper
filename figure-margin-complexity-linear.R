source("packages.R")

load("margin.complexity.linear.RData")

gg <- ggplot()+
  geom_text(aes(
    -log10(regularization),
    log10(margin),
    label=test.fold,
    color=variable),
    position="jitter",
    data=vstats.selected)+
  facet_grid(. ~ set.name)+
  directlabels::geom_dl(aes(
    -log10(regularization),
    log10(margin),
    color=variable,
    label=variable),
    data=vstats.selected,
    method="smart.grid")+
  guides(color="none")
print(gg)
pdf("figure-margin-complexity-linear-selected-parameters-scatter.pdf")
print(gg)
dev.off()

for(vname in unique(vstats$variable)){
  cost.dt <- vstats[variable==vname]
  min.dt <- vstats.min[variable==vname]
  selected.dt <- vstats.selected[variable==vname]
  if(vname=="negative.auc"){
    cost.dt[, mean := mean+1]
  }
  psize <- 2
  gg <- ggplot()+
    ggtitle(vname)+
    facet_grid(set.name ~ test.fold)+
    geom_point(aes(
      -log10(regularization),
      log10(margin),
      fill=log10(mean)),
      shape=21,
      size=psize,
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
      log10(margin),
      shape=type),
      data=rbind(
        data.table(min.dt, type="min"),
        data.table(selected.dt, type="selected")),
      fill="black",
      alpha=0.5,
      size=psize)+
    scale_shape_manual(values=c(min=1, selected=21))
  print(gg)
  pdf(paste0("figure-margin-complexity-", vname, ".pdf"))
  print(gg)
  dev.off()
}

eval.metrics[, accuracy.percent := 100-error.percent]
eval.tall <- melt(
  eval.metrics,
  measure.vars=c("auc", "accuracy.percent"),
  variable.name="metric.name",
  value.name="metric.value")
eval.scatter <- dcast(
  eval.tall,
  variable + test.fold + metric.name ~ set.name,
  value.var="metric.value")
eval.scatter[, {
  L <- t.test(dense, wide, paired=TRUE)
  with(L, data.table(
    p.value=ifelse(is.finite(p.value), p.value, 1),
    estimate
  ))
}, by=list(variable, metric.name)]
ggplot()+
  facet_wrap("metric.name", scales="free")+
  geom_abline(slope=1, intercept=0, color="white")+
  coord_equal()+
  geom_point(aes(
    dense, wide, color=variable),
    shape=1,
    data=eval.scatter)

eval.means <- eval.tall[, list(
  mean.metric=mean(metric.value)
), by=list(set.name, metric.name, variable)]
auc.means <- eval.means[metric.name=="auc" & set.name=="wide"]
levs <- auc.means[order(mean.metric), variable]
eval.tall[, validation.metric := factor(variable, levs)]
eval.means[, validation.metric := factor(variable, levs)]
gg.metrics <- ggplot()+
  ggtitle("5-fold CV test accuarcy on neuroblastoma data set")+
  geom_point(aes(
    mean.metric, validation.metric, color=set.name),
    alpha=0.5,
    size=4,
    data=eval.means)+
  geom_point(aes(
    metric.value, validation.metric, color=set.name),
    data=eval.tall,
    shape=1)+
  theme_bw()+
  theme_no_space()+
  facet_grid(. ~ metric.name, scales="free")+
  xlab("test accuracy")
print(gg.metrics)
pdf("figure-margin-complexity-linear.pdf", 7, 2)
print(gg.metrics)
dev.off()
