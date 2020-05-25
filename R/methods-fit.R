#' Build and train the SM.dlm Class in Package sm4sd
#'
#' Creating an object of \code{SM.dlm.fitted} class by build and train the dlm model based on the information
#'  of input.
#'
#' @param object An object of \code{\link{SM.dlm}}.
#'
#' @note Check the section \code{Examples} in \code{\link{SM.dlm.fitted}} for practical use.
#'
#' @name fit
#' @docType methods
#' @rdname fit-method
#' @include internal_fit.R
#' @exportMethod  fit
setGeneric("fit", function(object) standardGeneric("fit"))

#' @rdname fit-method
setMethod(f = "fit", signature = c("SM.dlm"), definition = .SM.dlm.fit)
