#' Show an Object
#'
#' Display the object by printing according to the class object. Check "Details" section for more info
#'  in each case.
#'
#' @param object An s4 class.
#' @section Details:
#' Simplying the print for each case:\itemize{
#'  \item{\code{\link{RS}}: }{first 25 elements for each slot;}
#'  \item{\code{\link{SM.dlm}}: }{call, seasonality of time series, index of event indicators, extra parameter as
#'   lagMax, verify and parallel, and inherited part of \code{\link{RS}} class.}
#'  \item{\code{\link{SM.dlm.fitted}}: } {call, best model parameters including dlm parameter (V, W, outlier
#'   ajustment), and associated lag, seasonality, index of event indicators and first 5 rows of tracking
#'   ordered decreasely by performance.}
#'  \item{\code{\link{SM.HL}}: } {call, corresponding offset and inherited part of \code{\link{RS}} class.}
#' }
#' @name show
#' @docType methods
#' @rdname show-method
#' @importMethodsFrom  methods show
#' @include internal_show.R
#' @aliases show,RS-method
setMethod(f = "show", signature = "RS", definition = .RS.show)

#' @rdname show-method
setMethod(f = "show", signature = "SM.dlm", definition = .SM.dlm.show)

#' @rdname show-method
setMethod(f = "show", signature = "SM.dlm.fitted", definition = .SM.dlm.fitted.show)

#' @rdname show-method
setMethod(f = "show", signature = "SM.HL", definition = .SM.HL.show)

#' @rdname show-method
setMethod(f = "show", signature = "SM.rnn", definition = .SM.rnn.show)
