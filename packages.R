### Write down what package versions work with your R code, and
### attempt to download and load those packages. The first argument is
### the version of R that you used, e.g. "3.0.2" and then the rest of
### the arguments are package versions. For
### CRAN/Bioconductor/R-Forge/etc packages, write
### e.g. RColorBrewer="1.0.5" and if RColorBrewer is not installed
### then we use install.packages to get the most recent version, and
### warn if the installed version is not the indicated version. For
### GitHub packages, write "user/repo@commit"
### e.g. "tdhock/animint@f877163cd181f390de3ef9a38bb8bdd0396d08a4" and
### we use install_github to get it, if necessary.
works_with_R <- function(Rvers,...){
  local.lib <- file.path(getwd(), "library")
  dir.create(local.lib, showWarnings=FALSE, recursive=TRUE)
  .libPaths(local.lib)
  pkg_ok_have <- function(pkg,ok,have){
    stopifnot(is.character(ok))
    if(!as.character(have) %in% ok){
      warning("works with ",pkg," version ",
              paste(ok,collapse=" or "),
              ", have ",have)
    }
  }
  pkg_ok_have("R",Rvers,getRversion())
  pkg.vers <- list(...)
  for(pkg.i in seq_along(pkg.vers)){
    vers <- pkg.vers[[pkg.i]]
    pkg <- if(is.null(names(pkg.vers))){
      ""
    }else{
      names(pkg.vers)[[pkg.i]]
    }
    if(pkg == ""){# Then it is from GitHub.
      ## suppressWarnings is quieter than quiet.
      if(!suppressWarnings(require(requireGitHub))){
        ## If requireGitHub is not available, then install it using
        ## devtools.
        if(!suppressWarnings(require(devtools))){
          install.packages("devtools")
          require(devtools)
        }
        install_github("tdhock/requireGitHub")
        require(requireGitHub)
      }
      requireGitHub(vers)
    }else{# it is from a CRAN-like repos.
      if(!suppressWarnings(require(pkg, character.only=TRUE))){
        install.packages(pkg)
      }
      pkg_ok_have(pkg, vers, packageVersion(pkg))
      library(pkg, character.only=TRUE)
    }
  }
}
options(repos="http://cloud.r-project.org")
works_with_R(
  "3.3.3",
  data.table="1.10.4",
  tikzDevice="0.10.1",
  Segmentor3IsBack="2.0",
  ggrepel="0.6.5",
  doParallel="1.0.6",
  partykit="2.0.2",#R CMD INSTALL partykit/pkg/devel/partykit/
  libcoin="0.9.1",
  mlt="0.1.3",
  survival="2.41.2",#this version is important! otherwise trtf wont work!
  "tdhock/penaltyLearning@1c36935f2984541e793b9d8de27c3060fd208a41",
  "faizan-khan-iit/ggplot2@5fb99d0cece13239bbbc09c6b8a7da7f86ac58e2",
  "tdhock/directlabels@8f717874c77edf20aeb3bd484381393958cef358",
  "tdhock/animint@9f3f84d8032f992784a4c0531ef117cdc2aa0d3f",
  trtf="0.1.1")#R CMD INSTALL ctm/pkg/trtf/  
requireGitHub::requireGitHub_package(
  "aldro61",
  "mmit/Rpackage",
  "edf81ba77fdd4b005ad89b81d9e12d289c8146e9",
  "mmit")
registerDoParallel()
options(
  tikzDocumentDeclaration=paste(
    "\\documentclass[12pt]{article}",
    "\\usepackage{amsmath,amssymb,amsthm}"),
  tikzMetricsDictionary="tikzMetrics")

