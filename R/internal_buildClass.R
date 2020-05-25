## Internal definition of buildClass methods
# .RS.buildClass.forecast <- function(object, h, call){
#   x1 <- as.vector(object@sigb)
#   x1.fore <- forecast(object = x1, h = h)
#
#   x2 <- as.vector(object@epsb)
#   x2.fore <- forecast(object = x2, h = h)
#
#   x3 <- as.vector(object@epsp)
#   x3.fore <- forecast(object = x3, h = h)
#
#   class(x1.fore) <- "list"
#   class(x2.fore) <- "list"
#   class(x3.fore) <- "list"
#
#   class.fore <- RS.AutoFore(call = call,
#                             sigb.fore = x1.fore, epsb.fore = x2.fore, epsp.fore = x3.fore,
#                             object)
#
#   class.fore
# }

.RS.buildClass.buildDLM <- function(object, freq, index, lagMax, verify, parallel, call){
  class.dlm <- SM.dlm(call = call,
                      freq = freq,
                      indicators = index,
                      lagMax = lagMax,
                      verify = verify,
                      parallel = parallel,
                      object)
  class.dlm
}

.RS.buildClass.buildHL <- function(object, offset, call){
  class.HL <- SM.HL(call = call,
                    offset = offset ,
                    object)
  class.HL
}

# method is character variable including follow options:
## forecast: build forecast model, additional parameter ind must be length 1 positive enter variable.
## dlm: build dlm model without fit, additional parameter ind must be a list indicates index slot.
## mp: build Magnus Persson which it is a deterministic model, additional parameter ind
### could be a length 1 numeric variable or a numeric vector. For moment, it should be a length 1.
.RS.buildClass <- function(object, method, freq, ind, lagMax, verify, parallel){
  call <- match.call()

  method <- tolower(method)
  if( !(method %in% c("forecast", "dlm", "hl")) ) {
    stop("No method has define por this index.")
  }
  # if( method == "forecast" ){
  #   h <- ind
  #   if(length(h) != 1 | h <= 0 | h != round(h)){
  #     stop("The forecast step should be positive length one integer variable.")
  #   }
  #
  #   mod <- .RS.buildClass.forecast(object = object, h = h, call = call)
  # }
  if( method == "dlm" ){
    index <- ind
    if( !is.numeric(freq) |  freq %% 1 != 0 | freq < 0 ){
      stop("Frequency must a integer value bigger o equal to 0.")
    }
    if( !is.list(index) ){
      stop("The event should be a list.")
    }
    if(!is.numeric(lagMax) |  length(lagMax) != 1){
      stop("lagMax must be length one numeric variable.")
    }
    if(!is.logical(verify) | !is.logical(parallel)){
      stop("verify & parallel must be logical.")
    }

    mod <- .RS.buildClass.buildDLM(call = call, object = object, freq = freq, index = index,
                        lagMax = lagMax, verify = verify, parallel = parallel)
  }
  if( method == "hl" ){
    offset <- ind
    if( length(offset) != 1 | offset < 0 ){
      stop("The offset should be no negative length one numeric variable.")
    }

    mod <- .RS.buildClass.buildHL(call = call, object = object, offset = offset)
  }

  mod
}
