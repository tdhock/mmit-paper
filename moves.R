source("packages.R")

targets.csv.vec <- Sys.glob(file.path("data", "*", "targets.csv"))
moves.list <- list()
for(data.i in seq_along(targets.csv.vec)){
  targets.csv <- targets.csv.vec[[data.i]]
  targets.dt <- fread(paste("sed 's/inf/Inf/'", shQuote(targets.csv)))
  data.dir <- dirname(targets.csv)
  data.name <- basename(data.dir)
  features.csv <- file.path(data.dir, "features.csv")
  features.dt <- fread(features.csv)
  data.moves.RData <- file.path(data.dir, "data.moves.RData")
  if(file.exists(data.moves.RData)){
    load(data.moves.RData)
  }else{
    data.moves.list <- list()
    cat(sprintf(
      "%4d / %4d %s n=%d p=%d\n",
      data.i, length(targets.csv.vec), data.name,
      nrow(features.dt), ncol(features.dt)))
    target.mat <- as.matrix(targets.dt)
    n.finite <- rowSums(is.finite(target.mat))
    total.finite <- sum(n.finite)
    stopifnot(is.numeric(target.mat))
    finite.vec <- sort(target.mat[is.finite(target.mat)])
    diff.vec <- diff(finite.vec)
    margin.vec <- exp(seq(
      log(min(diff.vec[0 < diff.vec])),
      log(max(finite.vec)-min(finite.vec)),
      l=10))
    for(feature.name in names(features.dt)){
      for(direction in c("increasing", "decreasing")){
        ord.vec <- order(
          features.dt[[feature.name]],
          decreasing=direction=="decreasing")
        ord.mat <- target.mat[ord.vec, ]
        for(margin in margin.vec){
          for(loss in c("hinge", "square")){
            result.df <- mmit::compute_optimal_costs(target.mat, margin, loss)
            moves.per.limit <- result.df$moves / n.finite
            data.moves.list[[paste(
              data.name, feature.name,
              direction, margin, loss)]] <- data.table(
                data.name, feature.name,
                direction, margin, loss,
                observations=nrow(target.mat),
                max.moves=max(result.df$moves),
                max.moves.per.limit=max(moves.per.limit),
                total.finite,
                mean.moves=sum(result.df$moves)/total.finite)
          }
        }
      }
    }
    data.moves <- do.call(rbind, data.moves.list)
    save(data.moves, file=data.moves.RData)
  }
  moves.list[[data.name]] <- data.moves
}
moves <- do.call(rbind, moves.list)

save(moves, file="moves.RData")
