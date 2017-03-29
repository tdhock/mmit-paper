source("packages.R")

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
data.set.sizes[order(percent.upper), list(
  set.name, upper.limits, lower.limits, percent.upper)]

fat.tall <- data.table(
  observations=c(1000, 100),
  features=c(10, 1000),
  label=c("tall data", "fat data"))
gg <- ggplot(data.set.sizes, aes(observations, features))+
  ggtitle(paste(
    nrow(data.set.sizes),
    "penalty learning interval regression data sets"))+
  theme_bw()+
  geom_abline(slope=1, intercept=0, color="grey")+
  geom_text(aes(label=label), data=fat.tall, color="grey50")+
  theme(
    ## legend.direction="horizontal",
    ## legend.box="horizontal",
    ## legend.position="bottom",
    panel.grid.minor=element_blank())+
  geom_text_repel(aes(label=set.name, color=model))+
  geom_point(aes(fill=percent.upper), shape=21, size=4)+
  guides(fill=guide_legend())+
  scale_fill_gradient2(
    "class balance
(percent
upper limits)",
    midpoint=50,
    breaks=data.set.sizes[, c(
      floor(max(percent.upper)),
    seq(70, 10, by=-10),
      ceiling(min(percent.upper))
    )])+
  scale_x_log10(
  breaks=c(
    range(data.set.sizes$observations),
    c(10, 100, 1000)
  ))+
  scale_y_log10(
   breaks=c(
    range(data.set.sizes$features),
    c(10, 100, 1000)),
   limits=c(10, 1000))+
  coord_equal()
png("figure-data-set-sizes.png", 10, 7, units="in", res=100)
print(gg)
dev.off()
