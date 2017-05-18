source("packages.R")

load("complexity.linear.RData")

selected.means <- vstats.selected[, list(
  mean=mean(regularization)
), by=list(variable)]
vlevs <- selected.means[order(mean), variable]
vstats.selected[, variable.fac := factor(variable, vlevs)]
gg <- ggplot()+
  geom_text(aes(
    -log10(regularization),
    variable.fac,
    label=test.fold),
    data=vstats.selected)
print(gg)
pdf("figure-complexity-linear-selected-parameters.pdf")
print(gg)
dev.off()

eval.metrics[, accuracy.percent := 100-error.percent]
eval.tall <- melt(
  eval.metrics,
  measure.vars=c("auc", "accuracy.percent"),
  variable.name="metric.name",
  value.name="metric.value")
eval.means <- eval.tall[, list(
  mean.metric=mean(metric.value)
), by=list(metric.name, variable)]
auc.means <- eval.means[metric.name=="auc"]
levs <- auc.means[order(mean.metric), variable]
eval.tall[, validation.metric := factor(variable, levs)]
eval.means[, validation.metric := factor(variable, levs)]
gg.metrics <- ggplot()+
  ggtitle(paste(
    "5-fold CV test accuarcy on neuroblastoma data set",
    "margin=1, regularization chosen using validation.metric",
    sep="\n"))+
  geom_point(aes(
    mean.metric, validation.metric),
    alpha=0.5,
    size=4,
    data=eval.means)+
  geom_point(aes(
    metric.value, validation.metric),
    data=eval.tall,
    shape=1)+
  theme_bw()+
  theme_no_space()+
  facet_grid(. ~ metric.name, scales="free")+
  xlab("test accuracy")
print(gg.metrics)
pdf("figure-complexity-linear.pdf", 7, 2)
print(gg.metrics)
dev.off()
