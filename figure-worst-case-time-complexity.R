source("packages.R")

size <- 20
n <- 1:size
n.vec <- (-n)^n ## N-2 moves.
n.vec <- (-10)^n ## N-2 moves as well.
##n.vec <- (-1)^n
out.mat <- cbind(
  ifelse(n.vec<0, -Inf, n.vec),
  ifelse(n.vec<0, n.vec, Inf))
target.mat.list <- list(
  "-10^n"=out.mat,
  "shuffled"=out.mat[sample(n),])
moves.dt <- data.table(data.name=names(target.mat.list))[, {
  target.mat <- target.mat.list[[data.name]]
  result.df <- mmit::compute_optimal_costs(target.mat, 1, "square")
  data.table(data.i=1:nrow(result.df), result.df)
}, by=list(data.name)]

with.legend <- ggplot()+
  geom_line(aes(
    data.i, moves,
    color=data.name),
    data=moves.dt)+
  scale_x_continuous(
    "number of data",
    breaks=c(1, 10, 20),
    limits=c(1, 25))
dl <- direct.label(with.legend, "last.polygons")
print(dl)

pdf("figure-worst-case-time-complexity.pdf")
print(dl)
dev.off()
