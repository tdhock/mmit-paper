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
options(repos=c(
          "http://r-forge.r-project.org",
          "http://cloud.r-project.org"))
if(packageVersion("survival") < "2.41.2"){
  install.packages("survival")
}
install.old.cran <- function(tgz.file){
  pkg <- sub("_.*", "", tgz.file)
  u <- paste0(
    "https://cran.r-project.org/src/contrib/Archive/",
    pkg, "/", tgz.file)
  if(!file.exists(tgz.file)){
    download.file(u, tgz.file)
  }
  install.packages(tgz.file, repos=NULL)
}
works_with_R("3.3.3", mvtnorm="1.0.6")
if(suppressWarnings(!require(libcoin))){
  install.old.cran("libcoin_0.9-2.tar.gz")
}

works_with_R(
  "3.3.3",
  data.table="1.10.4",
  tikzDevice="0.10.1",
  Segmentor3IsBack="2.0",
  quadprog="1.5.5",
  partykit="1.2.0",
  future="1.5.0",
  mlt="0.2.0",
  basefun="0.0.38",
  microbenchmark="1.4.2.1",
  httr="1.2.1",
  survival=c("2.41.2", "2.41.3"),#this version is important! otherwise trtf wont work!
  "faizan-khan-iit/ggplot2@5fb99d0cece13239bbbc09c6b8a7da7f86ac58e2",  # This needs to be before penaltyLearning
  geometry="0.4.0", #for penaltyLearning
  "tdhock/penaltyLearning@2abd159298f6573b0674d6c0efa13d27418bd90b",
  "anujkhare/iregnet@93729931a69f896dd752f5065c0d2e8859ed161f",
  "tdhock/directlabels@8f717874c77edf20aeb3bd484381393958cef358",
  RJSONIO="1.3.0", hexbin="1.27.1", #for animint
  "tdhock/animint@9f3f84d8032f992784a4c0531ef117cdc2aa0d3f")

set_config(config(ssl_verifypeer = 0L))

if(!require(trtf)){
  if(!file.exists("ctm")){
    system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ctm/")
  }
  system("cd ctm/pkg && svn up -r 734 && R CMD INSTALL -l ../../library trtf")
}

requireGitHub::requireGitHub_package(
  "aldro61",
  "mmit/Rpackage",
  "360bff413fbf209d3283b71b1c52bdfd7ab2c81a",
  "mmit")
options(
  tikzDocumentDeclaration=paste(
    "\\documentclass[12pt]{article}",
    "\\usepackage{amsmath,amssymb,amsthm}"),
  tikzMetricsDictionary="tikzMetrics")

