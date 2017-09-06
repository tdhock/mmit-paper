source("packages.R")

load("evaluate.predictions.RData")

evaluate.predictions[, min.metric := ifelse(grepl("MinIncorrect|constant", model.name), "01loss", "MSE")]
evaluate.predictions[, Model := sub("MinIncorrect", "", model.name)]

load("data.sets.RData")

evaluate.predictions[, accuracy.percent := 100 - error.percent]
min.nonzero <- evaluate.predictions[mean.squared.error!=0, min(mean.squared.error)]
evaluate.predictions[, `-log10.mean.squared.error` := -log10(mean.squared.error+min.nonzero)]
trafo.bad.wide <- evaluate.predictions[set.name=="H3K27ac-H3K4me3_TDHAM_BP_FPOP"]
trafo.bad.tall <- melt(trafo.bad.wide, measure.vars=c("auc", "accuracy.percent", "-log10.mean.squared.error"))
trafo.bad.gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ variable, scales="free")+
  geom_point(aes(
    ifelse(value < -5, -Inf, value), Model),
    data=trafo.bad.tall,
    shape=1)+
  xlab("")
print(trafo.bad.gg)
pdf("figure-evaluate-predictions-one-H3K27ac-H3K4me3_TDHAM_BP_FPOP.pdf", 8, 2)
print(trafo.bad.gg)
dev.off()
