source("packages.R")

data.dir.vec <- Sys.glob(file.path("data", "*"))
data.sets <- list()
for(set.i in seq_along(data.dir.vec)){
  data.dir <- data.dir.vec[[set.i]]
  set.name <- basename(data.dir)
  csv.file.vec <- Sys.glob(file.path(data.dir, "*.csv"))
  for(file.i in seq_along(csv.file.vec)){
    csv.file <- csv.file.vec[[file.i]]
    data.type <- sub("[.]csv$", "", basename(csv.file))
    mat <- as.matrix(fread(paste("sed 's/inf/Inf/'", shQuote(csv.file))))
    stopifnot(is.numeric(mat))
    data.sets[[set.name]][[data.type]] <- mat
  }
}

save(data.sets, file="data.sets.RData")
