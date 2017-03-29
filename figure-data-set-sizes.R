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
      grepl("joint", set.name), "joint", ifelse(
        grepl("FPOP", set.name), "FPOP", ifelse(
          grepl("PDPA", set.name), "PDPA", "copy#"))))
}, by=set.name]
data.set.sizes[, percent.upper := 100*upper.limits/(upper.limits+lower.limits)]
data.set.sizes[order(percent.upper), list(
  set.name, upper.limits, lower.limits, percent.upper)]

gg <- ggplot(data.set.sizes, aes(observations, features))+
  theme_grey()+
  theme(
    ## legend.direction="horizontal",
    ## legend.box="horizontal",
    ## legend.position="bottom",
    panel.grid.minor=element_blank())+
  geom_text_repel(aes(label=set.name, color=model))+
  geom_point(aes(fill=percent.upper), shape=21, size=4)+
  ggtitle(paste(
    nrow(data.set.sizes),
    "interval regression data sets"))+
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
  scale_x_log10()+
  scale_y_log10(limits=c(10, 1000))+
  coord_equal()
png("figure-data-set-sizes.png", 10, 7, units="in", res=100)
print(gg)
dev.off()
