source("packages.R")

data(neuroblastomaProcessed)
tmat <- neuroblastomaProcessed$target.mat
fmat <- neuroblastomaProcessed$feature.mat
n.vec <- as.integer(10^seq(3, 7, by=0.5))
set.seed(1)
i.vec <- sample(1:nrow(tmat), max(n.vec), replace=TRUE)
max.tmat <- tmat[i.vec, ]+runif(max(n.vec))
max.fmat <- fmat[i.vec, 1]+runif(max(n.vec))

arg.list <- list(times=10)
for(n.sim in n.vec){
  for(loss in c("hinge", "square")){
    arg.list[[paste(n.sim, loss)]] <- substitute({
      compute_optimal_costs(max.tmat[1:n.sim, ], 1, loss)
    }, list(loss=loss, n.sim=n.sim))
  }
  arg.list[[paste(n.sim, "sort")]] <- substitute({
    sort(max.fmat[1:n.sim])
  }, list(n.sim=n.sim))
}
m.result <- do.call(microbenchmark, arg.list)

result.dt <- data.table(m.result)
pattern <- paste0(
  "(?<data>[0-9]+)",
  " ",
  "(?<loss>.*)")
match.df <- namedCapture::str_match_named(
  paste(result.dt$expr), pattern, list(data=as.integer))
(match.result <- data.table(match.df, result.dt))

match.result[, hinge.loss := ifelse(loss=="hinge", "linear", loss)]
match.result[, seconds := time/1e9]
stats.dt <- match.result[, list(
  q75=quantile(seconds, 0.75),
  median=median(seconds),
  q25=quantile(seconds, 0.25)
  ), by=list(data, hinge.loss)]

gg.seconds <- ggplot()+
  ggtitle("Timings on simulated data sets")+
  theme_bw()+
  scale_x_log10(
    limits=c(NA, 1e8),
    breaks=10^(4:7),
    "number of outputs (finite interval limits)")+
  geom_line(aes(
    data, median,
    color=hinge.loss),
    size=1,
    data=stats.dt)+
  geom_ribbon(aes(
    data,
    fill=hinge.loss,
    ymin=q25, ymax=q75),
    data=stats.dt,
    alpha=0.5)+
  scale_y_log10(
    "seconds",
    breaks=10^seq(-2, 1))
print(dl <- direct.label(gg.seconds, "last.polygons"))
pdf("figure-simulated-seconds.pdf", 3.5, 3)
print(dl)
dev.off()

