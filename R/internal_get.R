## Internal definition of get methods

.RS.get <- function(x,i,j,drop){
  switch(EXPR = i,
         sigb = return(x@sigb),
         epsb = return(x@epsb),
         epsp = return(x@epsp),
         stop("Error:",i,"is not a RS slot")
  )
}

.RS.AutoFore.get <- function(x,i,j,drop){
  switch(EXPR = i,
         sigb.fore = return(x@sigb.fore),
         epsb.fore = return(x@epsb.fore),
         epsp.fore = return(x@epsp.fore),
         sigb = return(x@sigb),
         epsb = return(x@epsb),
         epsp = return(x@epsp),
         stop("Error:",i,"is not a RS.AutoFore slot")
  )
}

.SM.dlm.get <- function(x,i,j,drop){
  switch(EXPR = i,
         call = return(x@call),
         freq = return(x@freq),
         indicators = return(x@indicators),
         lagMax = return(x@lagMax),
         verify = return(x@verify),
         parallel  = return(x@parallel),
         sigb = return(x@sigb),
         epsb = return(x@epsb),
         epsp = return(x@epsp),
         stop("Error:",i,"is not a SM.dlm slot")
  )
}

.SM.dlm.fitted.get <- function(x,i,j,drop){
  switch(EXPR = i,
         call = return(x@call),
         parameters = return(x@parameters),
         filtered = return(x@filtered$m),
         smoothed = return(x@smoothed$s),
         lags = return(x@lags),
         seasonalSign = return(x@seasonalSign),
         tracking = return(x@tracking),
         freq = return(x@freq),
         indicators = return(x@indicators),
         lagMax = return(x@lagMax),
         verify = return(x@verify),
         parallel  = return(x@parallel),
         sigb = return(x@sigb),
         epsb = return(x@epsb),
         epsp = return(x@epsp),
         stop("Error:",i,"is not a SM.dlm slot")
  )
}

.SM.HL.get <- function(x,i,j,drop){
  switch(EXPR = i,
         offset = return(x@offset),
         sigb = return(x@sigb),
         epsb = return(x@epsb),
         epsp = return(x@epsp),
         stop("Error:",i,"is not a SM.dlm slot")
  )
}


.SM.rnn.get <- function(x,i,j,drop){

  switch(EXPR = i,
         call = return(x@call),
         model_path = return(x@model_path),
         compile_options = return(x@compile_options),
         optim_hyperparameters = return(x@optim_hyperparameters),
         preprocess_parameter = return(x@preprocess_parameter),
         tracking = return(x@tracking),
         stop("Error:",i,"is not a SM.rnn slot")
  )
}
