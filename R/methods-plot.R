#' Methods for Function Plot in Package sm4sd
#'
#' Plot slots of \code{\link{RS}} class using "timeSeries" plot.
#'
#' @param x An object of \code{\link{RS}} class.
#' @param y Missing object.
#' @param ... Additional graphical arguments, see plot, plot.default and par.
#'
#' @examples
#' n <- 50
#' # An example to create the RS class.
#' my_first_class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                      epsb = rnorm(n, mean = 0, sd = 2),
#'                      epsp = rnorm(n, mean = 1, sd = 1))
#'
#' # plot the class as timeseries object
#' plot(my_first_class)
#'
#' @name plot
#' @docType methods
#' @rdname plot-method
#' @include internal_plot.R
#' @exportMethod plot
#' @aliases plot,RS,missing-method

setMethod(f = "plot", signature = c("RS", "missing"), definition = .RS.plot)
