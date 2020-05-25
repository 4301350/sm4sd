#' Build Extended S4 Class
#'
#' \code{buildClass} extends the class \code{\link{RS}} to \code{\link{SM.dlm}} and \code{\link{SM.HL}}
#'  by adding the additional informations.
#'
#' @param object An object of \code{\link{RS}} class.
#' @param method An character value indicating the method to apply. Available method in this package are:
#'  \itemize{
#'   \item{dlm: }{build a \code{\link{SM.dlm}} class;}
#'   \item{hl: }{build a \code{\link{SM.HL}} class.}
#'  }
#' @param freq Object with type ANY. \itemize{
#'  \item{dlm method: }{a positive intervalue indicating the seasonality of the time series' structure;}
#'  \item{hl method: }{a NULL value, it is not necessary for \code{\link{SM.HL}} class.}
#' }
#' @param ind R object with type ANY. \itemize{
#'  \item{dlm method: }{a list containing outliers or human intervention indexes;}
#'  \item{hl method: }{a numeric value indicating the offset value.}
#' }
#' @param lagMax R object with type ANY. For dlm method, \code{lagMax} is an integer value indicating
#'  the maximum effect lag for each outlier. Check section Details in \code{\link{SM.dlm}}
#'  for more information.
#' @param verify R object with type ANY. For dlm method, \code{verify} is a logical value indicating
#'  whether the validation process should be applied. Check section Details in \code{\link{SM.dlm}}
#'  for more information.
#' @param parallel R object with type ANY. For dlm method, \code{verify} is a logical value indicating
#'  whether the parallel process should be applied.
#'
#' @examples
#' n <- 50
#' # An example to create the RS class.
#' my_first_class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                      epsb = rnorm(n, mean = 0, sd = 2),
#'                      epsp = rnorm(n, mean = 1, sd = 1))
#'
#' # build class
#' ## build dlm model
#' dlm_class <- buildClass(object = my_first_class,
#'                         method = "dlm", freq = 1,
#'                         ind = list(), lagMax = 3,
#'                         verify = TRUE, parallel = TRUE)
#' ## build HL model
#' hl_class <- buildClass(object = my_first_class,
#'                        method = "HL", ind = 9.5)
#' ### ind in this case is offset
#' @name buildClass
#' @docType methods
#' @rdname buildClass-method
#' @include internal_buildClass.R 3_classes.R
#' @exportMethod buildClass
setGeneric("buildClass", function(object, method, freq, ind, lagMax, verify, parallel)
  standardGeneric("buildClass"))

#' @rdname buildClass-method
setMethod(f = "buildClass", signature = c("RS", "character", "ANY", "ANY", "ANY", "ANY", "ANY"),
          definition = .RS.buildClass)
