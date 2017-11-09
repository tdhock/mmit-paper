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
  "-10^n"=out.mat)
n.random <- 10
moves.dt <- data.table(data.name=names(target.mat.list))[, {
  target.mat <- target.mat.list[[data.name]]
  data.table(seed=n.random:0)[, {
    if(seed==0){
      shuffled.mat <- target.mat
      ordering <- "pathological"
    }else{
      set.seed(seed)
      shuffled.mat <- target.mat[sample(n),]
      ordering <- "random"
    }
    result.df <- mmit::compute_optimal_costs(shuffled.mat, 1, "square")
    data.table(
      data.i=1:nrow(result.df),
      ordering=factor(ordering, c("random", "pathological")),
      result.df)
  }, by=list(seed)]
}, by=list(data.name)]

with.legend <- ggplot()+
  ggtitle(paste(
    "Pathological sequence and",
    n.random,
    "random orderings"))+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ data.name)+
  geom_line(aes(
    data.i, runif(length(moves), -0.1, 0.1)+moves,
    group=-seed,
    color=ordering),
            size=1,
    data=moves.dt)+
  scale_x_continuous(
    "n = number of data",
    breaks=c(1, 10, 20),
    limits=c(1, 25))+
  scale_y_continuous(
    "number of pointer moves using squared hinge loss")
dl <- direct.label(with.legend, "last.polygons")
print(dl)

pdf("figure-worst-case-time-complexity.pdf")
print(dl)
dev.off()
