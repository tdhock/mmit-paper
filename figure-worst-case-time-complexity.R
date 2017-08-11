source("packages.R")

size <- 20
n <- 1:size
n.vec <- (-n)^n ## N-2 moves.
n.vec <- (-10)^n ## N-2 moves as well.
out.mat <- cbind(
  ifelse(n.vec<0, -Inf, n.vec),
  ifelse(n.vec<0, n.vec, Inf))
(result <- mmit::compute_optimal_costs(out.mat, 1, "square"))
plot(result$moves)
pdf("figure-worst-case-time-complexity.pdf")
plot(result$moves)
dev.off()
