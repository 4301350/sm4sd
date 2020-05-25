#' Extract Slots of an S4 Class
#'
#' Operators acting on existing classes to extract slot's values.
#'
#' @param x An S4 object from which to extact slot's values.
#' @param i A character value indicating the slot's name.
#' @param j Missing value which is not used for objects from this package.
#' @param drop Same as \code{j}.
#'
#' @examples
#' n <- 50
#' # An example to create the RS class.
#' my_first_class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                      epsb = rnorm(n, mean = 0, sd = 2),
#'                      epsp = rnorm(n, mean = 1, sd = 1))
#' # Extract the slot "sigb"
#' sigb <- my_first_class["sigb"]
#'
#' @name [
#' @docType methods
#' @rdname extract-method
#' @include internal_get.R
#' @importMethodsFrom  methods '['
#' @aliases [,RS,character,missing,missing-method

setMethod(f = "[", signature = c("RS", "character", "missing", "missing"),
          definition = .RS.get)

# @rdname extract-method
# setMethod(f = "[", signature = c("RS.AutoFore", "character", "missing", "missing"),
#          definition = .RS.AutoFore.get)

#' @rdname extract-method
setMethod(f = "[", signature = c("SM.dlm", "character", "missing", "missing"),
          definition = .SM.dlm.get)

#' @rdname extract-method
setMethod(f = "[", signature = c("SM.dlm.fitted", "character", "missing", "missing"),
          definition = .SM.dlm.fitted.get)

#' @rdname extract-method
setMethod(f = "[", signature = c("SM.HL", "character", "missing", "missing"),
          definition = .SM.HL.get)

#' @rdname extract-method
setMethod(f = "[", signature = c("SM.rnn", "character", "missing", "missing"), definition = .SM.rnn.get)
