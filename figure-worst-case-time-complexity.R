source("packages.R")

size <- 20
n <- 1:size
n.vec <- (-n)^n
out.mat <- cbind(n.vec, n.vec + abs(rnorm(size)))
(result <- mmit::compute_optimal_costs(out.mat, 1, "square"))
plot(result$moves)
