#' Residual Diagnostic for DLM Model
#'
#' Residual Diagnostic including several normal test and residual plots.
#'
#' @param object An object to be analyzed.
#' @param gamma Numeric value between 0 and 1 indicating the confidence level.
# @param ask A logical value indicating the plots should be asked.
#'
#' @section Details:
#'  The residual diagnostic include the confidence band plot, chisq test for normalized error plot,
#'  \code{\link[stats:qqplot]{Quantile-Quantile Plots}} and
#'  \code{\link[lmtest:dwtest]{Durbin-Waston Test for autocorrelation}}.
#'
#' @docType methods
#' @rdname residualDiag-method
#' @include internal_residualDiag.R
#' @exportMethod  residualDiag

setGeneric("residualDiag", function(object,  gamma) standardGeneric("residualDiag"))
#' @rdname residualDiag-method
setMethod(f = "residualDiag", signature = c("SM.dlm.fitted", "numeric"),
          definition = .SM.dlm.fitted.residualDiag)
