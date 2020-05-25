#' Correlation Plot
#'
#' Correlation plot for the \ifelse{html}{\out{x<sub>t</sub>}}{\eqn{x_t}} (product of \ifelse{html}{\out{&#949<sub>p</sub>, &#963<sub>b</sub>}}{\eqn{\epsilon_b, \sigma_p}{ASCII}})
#' and \ifelse{html}{\out{y<sub>t</sub>}}{\eqn{y_t}} (\ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}}) by groups.
#'
#' @param x An object of \code{\link{RS}} class.
#' @param groups A positive integer indicating the number of subsets with equal length and preserving the structure order. Valid values:
#' \itemize{
#'  \item{1: }{unic linear regresion;}
#'  \item{2 - 6: }{linear regresion by group depending the number of group;}
#'  \item{bigger than 6: }{exceeded group, pass to 6 groups.}
#' }
#' @param ... Additional graphical arguments, see plot, plot.default and par.
#'
#'
#' @examples
#' n <- 50
#' # An example to create the RS class.
#' my_first_class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                      epsb = rnorm(n, mean = 0, sd = 2),
#'                      epsp = rnorm(n, mean = 1, sd = 1))
#' # plot the correlation
#' plotCor(my_first_class, 7)
#' @name plotCor
#' @docType methods
#' @rdname plotCor-method
#' @include internal_plotCor.R
#' @exportMethod plotCor
setGeneric("plotCor", function(x, groups, ...) standardGeneric("plotCor"))

#' @rdname plotCor-method
setMethod(f = "plotCor", signature = c("RS", "numeric"), definition = .RS.plot.corr)
