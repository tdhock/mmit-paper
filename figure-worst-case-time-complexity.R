source("packages.R")

mmit::compute_optimal_costs(rbind(
  c(-Inf, 1),
  c(0, Inf),
  c(-Inf, -1000)
  ), 1, "square")

