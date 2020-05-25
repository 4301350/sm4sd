#' Extract and/or Build the Corresponding Model
#'
#' Rebuilding the model based on the informations stored in the object.
#'
#' @param object An existing class in this package.
#' @param name Missing value, it's not defined for current version.
#'
#' @note Check the section \code{Examples} in \code{\link{SM.dlm.fitted}} for practical use.
#'
#' @name getMod
#' @docType methods
#' @rdname getMod-method
#' @include internal_getMod.R
#' @exportMethod  getMod

setGeneric("getMod", function(object, name) standardGeneric("getMod"))


# # no documentar
# setMethod(f = "getMod", signature = c("RS.AutoFore", "character"),
#           definition = .RS.AutoFore.getMod)

#' @rdname getMod-method
setMethod(f = "getMod", signature = c("SM.dlm.fitted", "missing"),
          definition = .SM.dlm.fitted.getdlm)

