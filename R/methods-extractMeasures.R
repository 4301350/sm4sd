#' Extract and Plot Measurement
#'
#' Extract and plot the electrical conductivity of pore water.
#'
#' @param object Object to be extracted the stored measurement.
#' @param plot A logical value indicating whether the graphic should be plotted.
#'
#'
#' @note
#' Check the section \code{Examples} in \code{\link{SM.dlm.fitted}} and \code{\link{SM.HL}}
#' for practical use in each case.
#'
#' @name extractMeasures
#' @docType methods
#' @rdname extractMeasures-method
#' @include internal_extractMeasures.R
#' @exportMethod  extractMeasures

setGeneric("extractMeasures", function(object, plot) standardGeneric("extractMeasures"))

@rdname extractMeasures-method
setMethod(f = "extractMeasures", signature = c("SM.dlm", "logical"),
         definition = .SM.dlm.extractMeasures)

#' @rdname extractMeasures-method
setMethod(f = "extractMeasures", signature = c("SM.dlm.fitted", "logical"),
          definition = .SM.dlm.fitted.extractMeasures)
#' @rdname extractMeasures-method
setMethod(f = "extractMeasures", signature = c("SM.HL", "logical"),
          definition = .SM.HL.extractMeasures)
