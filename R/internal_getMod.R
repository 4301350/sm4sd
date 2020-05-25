#' @include internal_fit.R
## Internal definition of getMod methods


.RS.AutoFore.getMod <- function(object, name){
  name <- paste0(name, ".fore")
  fore <- object[name]
  fore$model
}


.SM.dlm.fitted.getdlm <- function(object){
  epsb <- object@epsb
  epsp <- object@epsp
  sigb <- object@sigb
  freq <- object@freq
  index <- object@indicators

  # Define xt and yt
  yt <- epsb
  xt <- epsp * sigb

  param <- unlist(object@parameters)
  lag_list <- object@lags
  build <- function(x){
    dlm <- .SM.dlm.buildFunction(x, xt, yt, freq, index, lag_list )
    dlm
  }
  dlmFit <- build(param)

  dlmFit
}
