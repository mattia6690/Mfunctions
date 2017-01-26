# R Initialization Functions MR


#' @title Loadandinstall
#' @description Loads and installs a CRAN hosted Package automatically in R.
#' No further need to apply both install.package() and library()
#' @param mypkg character; CRAN Package
#' @author Christian Zeidler, University Wuerzburg

loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
  {install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

#' @title dircheckup
#' @description Automatically checks whether a Directory exists. If none is
#' available it will automatically create one.
#' By specifying the Subdirectory also non-existing main directories will
#' be automatically built
#' @param mypkg character; Location of a Directory

dircheckup     <- function(mydir) {
  loadandinstall("stringr")
  sp<-str_split(mydir,"/")[[1]]
  for(i in 2:length(sp)){
    mydir2<-paste(sp[1:i],collapse="/")
    if(dir.exists(mydir2)==F){dir.create(mydir2)}
  }
}
