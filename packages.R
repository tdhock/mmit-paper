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

library(httr)
set_config(config(ssl_verifypeer = 0L))

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
  partykit="1.2.0",
  libcoin="0.9.1",
  mlt="0.1.3",
  future="1.4.0",
  microbenchmark="1.4.2.1",
  survival=c("2.41.2", "2.41.3"),#this version is important! otherwise trtf wont work!
  "faizan-khan-iit/ggplot2@5fb99d0cece13239bbbc09c6b8a7da7f86ac58e2",  # This needs to be before penaltyLearning
  "tdhock/penaltyLearning@1bb8446e5e37903ee2ba05d76267cc0a08f9d002",
  "anujkhare/iregnet@93729931a69f896dd752f5065c0d2e8859ed161f",
  "tdhock/directlabels@8f717874c77edf20aeb3bd484381393958cef358",
  "tdhock/animint@9f3f84d8032f992784a4c0531ef117cdc2aa0d3f")
requireGitHub::requireGitHub_package(
  "aldro61",
  "mmit/Rpackage",
  "f77cc3ed9c72f55d267cdb0dd3f757fa813dad72",
  "mmit")
registerDoParallel()
options(
  tikzDocumentDeclaration=paste(
    "\\documentclass[12pt]{article}",
    "\\usepackage{amsmath,amssymb,amsthm}"),
  tikzMetricsDictionary="tikzMetrics")

