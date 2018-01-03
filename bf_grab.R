bf_grab <- function(x) {
  logbf <- x@bayesFactor$bf
  bf <- exp(logbf)
  bf <- round(bf, 2)
  return(bf)
}