
#' @title Binary
#' @description Creates the Binary output of a number
#' @param i numeric; numeric to be transformed in a binaric array
#' @param bit numeric; number of digits for the binaric ountput
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
digitsum <- function(x) sum(floor(x / 10^(0:(8 - 1))) %% 10)
