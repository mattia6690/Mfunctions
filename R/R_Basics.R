
#' @title Binary
#' @description Creates the Binary output of a number
#' @param i numeric; numeric to be transformed in a binaric array
#' @param bit numeric; number of digits for the binaric ountput
#' @export

binary <- function(i,bit){

  bite<-bit-1
  a<-2^(0:bite)
  b<-2*a

  # Revert by Sum from 7 to 0 (7:0) -> Starting with Day 1 to 8 (MODIS codes from Day 8 to Day 1)
  sapply(i,function(x) sum(10^(bite:0)[(x %% b)>=a]))
}

#' @title Digit Sum
#' @description Computes the sum of digits in one number
#' @param x numeric; any numeric
#' @export

digitsum <- function(x) sum(floor(x / 10^(0:(8 - 1))) %% 10)


#' @title Multigrepl
#' @description Allows to add multiple patterns to a grepl operation.
#' These multiple patterns are then tested against other strings.
#' This function is based on the base grepl operator
#' @param mypatterns string(s); list of multiple input strings
#' @param x string(s); list of multiple target patterns
#' @export

multigrepl      <- function(mypatterns,x){

  # Initialize the For iteration for processing the relevant patterns
  # The x has to stay the same
  for(i in 1:length(mypatterns)){

    g1<-grepl(mypatterns[i],x)
    if(i==1){g2<-g1}
    g2[which(g1==T)]=TRUE

  }

  return(g2)

}
