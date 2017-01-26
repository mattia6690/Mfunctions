
#' @title Loadandinstall
#' @param mypkg CRAN Package
#' @description Loads and installs a CRAN hosted PAckage automatically in R.
#' No further need to apply both install.package() and library()


loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
  {install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

#' @title dircheckup
dircheckup     <- function(mydir) {
  loadandinstall("stringr")
  sp<-str_split(mydir,"/")[[1]]
  for(i in 2:length(sp)){
    mydir2<-paste(sp[1:i],collapse="/")
    if(dir.exists(mydir2)==F){dir.create(mydir2)}
  }
}
