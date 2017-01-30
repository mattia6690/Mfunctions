# R Initialization Functions MR
req.pkg<- c("raster","stringr","utils")
for(i in req.pkg){
  if (!is.element(i, installed.packages()[,1])){
    install.packages(i,repos = "http://cran.rstudio.com/")}; library(i, character.only=TRUE)
}

#' @title Loadandinstall
#' @description Loads and installs a CRAN hosted Package automatically in R.
#' No further need to apply both install.package() and library()
#' @param mypkg character; CRAN Package
#' @export

loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
  {install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

#' @title dircheckup
#' @description Automatically checks whether a Directory exists. If none is
#' available it will automatically create one.
#' By specifying the Subdirectory also non-existing main directories will
#' be automatically built
#' @param mydir character; Location of a Directory
#' @export

dircheckup     <- function(mydir) {

  sp<-str_split(mydir,"/")[[1]]
  for(i in 2:length(sp)){
    mydir2<-paste(sp[1:i],collapse="/")
    if(dir.exists(mydir2)==F){dir.create(mydir2)}
  }
}
