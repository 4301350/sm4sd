#' Print Values
#'
#' \code{print} class values.
#'
#' @param x An S4 class.
#' @param ... Additional argument for \code{\link[base]{print}} where is not used in this pakcage.
#' @name print
#' @docType methods
#' @rdname print-method
#'
#' @exportMethod print
#' @include internal_print.R
#' @aliases print,RS-method
setMethod(f = "print", signature = "RS", definition = .RS.print)

#' @rdname print-method
setMethod(f = "print", signature = "SM.dlm", definition = .SM.dlm.print)

#' @rdname print-method
setMethod(f = "print", signature = "SM.dlm.fitted", definition = .SM.dlm.fitted.print)

#' @rdname print-method
setMethod(f = "print", signature = "SM.HL", definition = .SM.HL.print)

#' @rdname print-method
setMethod(f = "print", signature = "SM.rnn", definition = .SM.rnn.print)
