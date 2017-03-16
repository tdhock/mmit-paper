source("packages.R")

data(neuroblastomaProcessed, package="penaltyLearning")

feature.name.vec <- c("log.hall", "log.n")
margin <- 1
for(feature.name in feature.name.vec){
  feature.vec <- neuroblastomaProcessed$feature.mat[, feature.name]
  ord <- order(feature.vec)
  feature.ord.vec <- feature.vec[ord]
  target.ord.mat <- neuroblastomaProcessed$target.mat[ord, ]
  result.fwd <- compute_optimal_costs(target.ord.mat, margin)
  target.rev.mat <- target.ord.mat[nrow(target.ord.mat):1, ]
  result.rev <- compute_optimal_costs(target.rev.mat, margin)
  both <- data.table(rev=result.rev[nrow(result.rev):1, ], result.fwd)
  both[, total.cost := cost + rev.cost]
  plot(both$total.cost)
}
