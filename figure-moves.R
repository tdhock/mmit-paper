source("packages.R")

load("moves.RData")

moves[, round.log10.limits := round(log10(total.finite), 1)]
moves[, hinge.loss := ifelse(loss=="hinge", "linear", "square")]
dmoves <- moves[, list(
  max.moves=max(max.moves),
  min.max=min(max.moves),
  max.moves.per.limit=max(max.moves.per.limit),
  mean.max=mean(max.moves.per.limit),
  sd.max=sd(max.moves.per.limit),
  mean.moves=mean(mean.moves),
  sd.moves=sd(mean.moves),
  median.moves=median(mean.moves),
  q75.moves=quantile(mean.moves, 0.75),
  q25.moves=quantile(mean.moves, 0.25),
  median.max=median(max.moves.per.limit),
  q75.max=quantile(max.moves.per.limit, 0.75),
  q25.max=quantile(max.moves.per.limit, 0.25)
), by=list(total.finite=10^round.log10.limits, loss, hinge.loss)]

gg.max <- ggplot()+
  theme_bw()+
  scale_x_log10(
    "number of outputs (finite interval limits)")+
  geom_line(aes(
    total.finite, median.max,
    color=hinge.loss),
    size=1,
    data=dmoves)+
  geom_ribbon(aes(
    total.finite,
    fill=hinge.loss,
    ymin=q25.max, ymax=q75.max),
    data=dmoves,
    alpha=0.5)+
  scale_y_continuous("max pointer moves", limits=c(0,9), breaks=0:9)
print(gg.max)
pdf("figure-moves-max.pdf", 4, 3)
print(gg.max)
dev.off()

gg <- ggplot()+
  theme_bw()+
  scale_x_log10(
    "number of outputs (finite interval limits)")+
  geom_line(aes(
    total.finite, median.moves,
    color=hinge.loss),
    size=1,
    data=dmoves)+
  geom_ribbon(aes(
    total.finite,
    fill=hinge.loss,
    ymin=q25.moves, ymax=q75.moves),
    data=dmoves,
    alpha=0.5)+
  scale_y_continuous("average pointer moves", limits=c(0,1))
print(gg)
pdf("figure-moves-average.pdf", 4, 3)
print(gg)
dev.off()

y <- function(yvar){
  factor(yvar, c("max", "average"))
}
gg <- ggplot()+
  ggtitle(paste(
    "max and average pointer moves over all",
    "data sets, features, and margin parameters",
    sep="\n"))+
  theme_bw()+
  facet_wrap("y", scales="free")+
  scale_x_log10("number of outputs (finite interval limits)")+
  ## geom_line(aes(
  ##   total.finite, mean.moves,
  ##   color=loss),
  ##   size=1,
  ##   data=data.table(dmoves, y="average"))+
  ## geom_ribbon(aes(
  ##   total.finite,
  ##   fill=loss,
  ##   ymin=mean.moves-sd.moves, ymax=mean.moves+sd.moves),
  ##   data=data.table(dmoves, y="average"),
  ##   alpha=0.5)+
  ## geom_line(aes(
  ##   total.finite, mean.max,
  ##   color=loss),
  ##   size=1,
  ##   data=data.table(dmoves, y="max"))+
  ## geom_ribbon(aes(
  ##   total.finite,
  ##   fill=loss,
  ##   ymin=mean.max-sd.max, ymax=mean.max+sd.max),
  ##   data=data.table(dmoves, y="max"),
  ##   alpha=0.5)+
  geom_line(aes(
    total.finite, median.moves,
    color=hinge.loss),
    size=1,
    data=data.table(dmoves, y=y("average")))+
  geom_ribbon(aes(
    total.finite,
    fill=hinge.loss,
    ymin=q25.moves, ymax=q75.moves),
    data=data.table(dmoves, y=y("average")),
    alpha=0.5)+
  geom_line(aes(
    total.finite, median.max,
    color=hinge.loss),
    size=1,
    data=data.table(dmoves, y=y("max")))+
  geom_ribbon(aes(
    total.finite,
    fill=hinge.loss,
    ymin=q25.max, ymax=q75.max),
    data=data.table(dmoves, y=y("max")),
    alpha=0.5)+
  ## geom_line(aes(
  ##   total.finite, max.moves.per.limit,
  ##   color=loss),
  ##   data=data.table(dmoves, y="max"))+
  scale_y_continuous("pointer moves", breaks=0:10)+
  geom_blank(aes(total.finite, moves), data=data.table(
    total.finite=100,
    moves=c(0, 1, 0, 10),
    y=y(c("average", "average", "max", "max"))))
print(gg)
pdf("figure-moves.pdf", 4.5, 3)
print(gg)
dev.off()

gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(y ~ ., scales="free")+
  scale_x_log10("number of outputs (finite interval limits)")+
  ## geom_line(aes(
  ##   total.finite, mean.moves,
  ##   color=loss),
  ##   size=1,
  ##   data=data.table(dmoves, y="average"))+
  ## geom_ribbon(aes(
  ##   total.finite,
  ##   fill=loss,
  ##   ymin=mean.moves-sd.moves, ymax=mean.moves+sd.moves),
  ##   data=data.table(dmoves, y="average"),
  ##   alpha=0.5)+
  ## geom_line(aes(
  ##   total.finite, mean.max,
  ##   color=loss),
  ##   size=1,
  ##   data=data.table(dmoves, y="max"))+
  ## geom_ribbon(aes(
  ##   total.finite,
  ##   fill=loss,
  ##   ymin=mean.max-sd.max, ymax=mean.max+sd.max),
  ##   data=data.table(dmoves, y="max"),
  ##   alpha=0.5)+
  geom_line(aes(
    total.finite, median.moves,
    color=loss),
    size=1,
    data=data.table(dmoves, y=y("average")))+
  geom_ribbon(aes(
    total.finite,
    fill=loss,
    ymin=q25.moves, ymax=q75.moves),
    data=data.table(dmoves, y=y("average")),
    alpha=0.5)+
  geom_line(aes(
    total.finite, median.max,
    color=loss),
    size=1,
    data=data.table(dmoves, y=y("max")))+
  geom_ribbon(aes(
    total.finite,
    fill=loss,
    ymin=q25.max, ymax=q75.max),
    data=data.table(dmoves, y=y("max")),
    alpha=0.5)+
  ## geom_line(aes(
  ##   total.finite, max.moves.per.limit,
  ##   color=loss),
  ##   data=data.table(dmoves, y="max"))+
  scale_y_continuous("pointer moves", breaks=0:10)+
  geom_blank(aes(total.finite, moves), data=data.table(
    total.finite=100,
    moves=c(0, 1, 0, 10),
    y=y(c("average", "average", "max", "max"))))
png("figure-moves.png")
print(gg)
dev.off()

