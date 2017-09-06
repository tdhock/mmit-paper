source("packages.R")

load("constant.predictions.RData")

wide.dt <- dcast(
  constant.predictions,
  set.name + fold ~ fun.name,
  value.var="pred.log.penalty")

wide.dt[order(constant-constant01)]

ggplot()+
  geom_abline(slope=1, intercept=0, color="grey")+
  geom_point(aes(
    constant01, constant),
    shape=1,
    data=wide.dt)+
  coord_equal()

gg <- ggplot()+
  ggtitle(paste(
    "constant predicted values are different",
    "one dot for each data set and train/test split",
    sep="\n"))+
  geom_abline(slope=1, intercept=0, color="grey")+
  geom_point(aes(
    constant, constantMSE),
    shape=1,
    data=wide.dt)+
  coord_equal()
print(gg)

pdf("figure-constant-predictions.pdf")
print(gg)
dev.off()

