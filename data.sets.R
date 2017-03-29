source("packages.R")

n.folds <- 5

makeData <- function(features, targets){
  stopifnot(nrow(features) == nrow(targets))
  set.seed(1)
  colnames(targets) <- c("min.log.penalty", "max.log.penalty")
  has.finite.limit <- apply(is.finite(targets), 1, any)
  list(
    features=features[has.finite.limit, apply(is.finite(features), 2, all)],
    targets=targets[has.finite.limit, ],
    folds=data.table(fold=sample(rep(1:n.folds, l=sum(has.finite.limit)))))
}

data(neuroblastomaProcessed, package="penaltyLearning")

data.sets <- with(neuroblastomaProcessed, {
  list(neuroblastomaProcessed=makeData(feature.mat, target.mat))
})

if(!file.exists("signal.list.annotation.sets.RData")){
  download.file("http://members.cbio.ensmp.fr/~thocking/neuroblastoma/signal.list.annotation.sets.RData", "signal.list.annotation.sets.RData")
}
load("signal.list.annotation.sets.RData")

max.segments <- 20
selection.list <- list()
segs.list <- list()
for(signal.i in seq_along(signal.list)){
  signal.name <- names(signal.list)[[signal.i]]
  cat(sprintf("%4d / %4d %s\n", signal.i, length(signal.list), signal.name))
  model.RData <- file.path("Segmentor", signal.name, "segs.selection.RData")
  if(file.exists(model.RData)){
    load(model.RData)
  }else{
    pro <- signal.list[[signal.name]]
    fit <- Segmentor3IsBack::Segmentor(
      pro$logratio, model=2, Kmax=max.segments)
    model.dt <- data.table(
      signal.name,
      loss=as.numeric(fit@likelihood),
      n.segments=1:max.segments)
    selection.dt <- data.table(modelSelection(
      model.dt, complexity="n.segments"))
    segs.dt <- selection.dt[, {
      end <- fit@breaks[n.segments, 1:n.segments]
      data.before.change <- end[-n.segments]
      data.after.change <- data.before.change+1
      pos.before.change <- as.integer(
        (pro$position[data.before.change]+pro$position[data.after.change])/2)
      start <- c(1, data.after.change)
      chromStart <- c(pro$position[1], pos.before.change)
      chromEnd <- c(pos.before.change, max(pro$position))
      data.table(
        signal.name,
        n.segments,
        start,
        end,
        chromStart,
        chromEnd,
        mean=fit@parameters[n.segments, 1:n.segments])
    }, by=n.segments]
    dir.create(dirname(model.RData), showWarnings=FALSE, recursive=TRUE)
    save(selection.dt, segs.dt, file=model.RData)
  }
  selection.list[[signal.name]] <- selection.dt
  segs.list[[signal.name]] <- segs.dt
}
segs <- do.call(rbind, segs.list)
selection <- do.call(rbind, selection.list)

signal.features <- tryCatch({
  readRDS("signal.features.rds")
}, error=function(e){
  signal.features.list <- list()
  for(signal.i in seq_along(signal.list)){
    signal.name <- names(signal.list)[[signal.i]]
    cat(sprintf("%4d / %4d %s\n", signal.i, length(signal.list), signal.name))
    pro <- signal.list[[signal.name]]
    signal.features.list[[signal.name]] <- featureVector(pro$logratio)
  }
  signal.features <- do.call(rbind, signal.features.list)
  saveRDS(signal.features, "signal.features.rds")
  signal.features
})

for(set.name in names(annotation.sets)){
  label.dt <- data.table(annotation.sets[[set.name]])
  label.dt[, signal.name := paste0(profile.id, ".", chromosome)]
  setkey(label.dt, signal.name, min, max)
  label.dt[, bases.to.next := c(min[-1]-max[-.N], NA), by=signal.name]
  ## for every pair of overlapping labels, remove the one on the left.
  not.overlap <- label.dt[0 < bases.to.next]
  error.list <- labelError(
    selection, not.overlap, segs[1 < start],
    problem.vars="signal.name"
    )
  targets <- targetIntervals(error.list$model.errors, "signal.name")
  data.sets[[set.name]] <- with(targets, makeData(
    signal.features[signal.name, ],
    cbind(min.log.lambda, max.log.lambda)
    ))
}

if(!file.exists("ChIPseq.wholeGenome.rds")){
  system("scp thocking@guillimin.hpc.mcgill.ca:PeakSegFPOP/ChIPseq.wholeGenome.rds .")
}
ChIPseq.wholeGenome <- readRDS("ChIPseq.wholeGenome.rds")

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

save(data.sets, file="data.sets.RData")
