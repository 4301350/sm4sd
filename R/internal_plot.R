## Internal definition of plot methods

.RS.plot <- function(x, y, ...){
  sigb <- x@sigb
  epsb <- x@epsb
  epsp <- x@epsp

  series <- data.frame(sigb = as.vector(sigb), epsb = as.vector(epsb), epsp = as.vector(epsp))
  timeSeries::plot(ts(series), plot.type = "multiple", main = "plot of 'sigb', 'epsb' and 'epsp'")
  invisible()
}
