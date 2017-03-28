source("packages.R")

n.folds <- 5

data.sets <- list()

if(!file.exists("signal.list.annotation.sets.RData")){
  download.file("http://members.cbio.ensmp.fr/~thocking/neuroblastoma/signal.list.annotation.sets.RData", "signal.list.annotation.sets.RData")
}
load("signal.list.annotation.sets.RData")
for(model.RData in names(ChIPseq.wholeGenome)){
  ## TODO run segmentation and compute target intervals.
}

if(!file.exists("ChIPseq.wholeGenome.rds")){
  system("scp thocking@guillimin.hpc.mcgill.ca:PeakSegFPOP/ChIPseq.wholeGenome.rds .")
}
ChIPseq.wholeGenome <- readRDS("ChIPseq.wholeGenome.rds")

makeData <- function(features, targets){
  set.seed(1)
  all.finite <- apply(is.finite(features), 2, all)
  list(
    features=features[, all.finite],
    targets=targets,
    folds=data.table(fold=sample(rep(1:n.folds, l=nrow(features)))))
}

for(model.RData in names(ChIPseq.wholeGenome)){
  new.name <- sub("[.].*", "", sub("/", "_", sub("labels/", "", model.RData)))
  set <- ChIPseq.wholeGenome[[model.RData]]
  data.sets[[new.name]] <- with(set, makeData(feature.mat, target.mat))
}

for(set.name in names(data.sets)){
  data.list <- data.sets[[set.name]]
  for(data.type in names(data.list)){
    out.dt <- data.table(data.list[[data.type]])
    out.csv <- file.path("data", set.name, paste0(data.type, ".csv"))
    dir.create(dirname(out.csv), showWarnings=FALSE, recursive=TRUE)
    fwrite(out.dt, out.csv)
  }
}
