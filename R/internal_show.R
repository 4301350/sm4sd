## Internal definition of show methods

.RS.show <- function(object){
  sigb <- round(object@sigb, 2)
  epsb <- round(object@epsb, 2)
  epsp <- round(object@epsp, 2)

  n1 <- length(sigb)
  n2 <- length(epsb)
  n3 <- length(epsp)

  cat("An object of class RS (RawSignals):\n\n")
  k <- 25
  cat(paste0("Length = ", n1, " (limited to a 25)\n"))

  if( n1 == 0){
    cat("\n* \"sigb\" = "); print(sigb)

    cat("\n* \"epsb\" = "); print(epsb)

    cat("\n* \"epsp\" = "); print(epsp)
  }else{
    cat(paste0("\n* \"sigb\"", " (", class(sigb), ") : \n"))
    print(sigb[1:k])

    cat(paste0("\n* \"epsb\"", " (", class(epsb), ") : \n"))
    print(epsb[1:k])

    cat(paste0("\n* \"epsp\"", " (", class(epsp), ") : \n"))
    print(epsp[1:k])
  }

  invisible()
}
.SM.dlm.show <- function(object){
  call <- object@call
  freq <- object@freq
  ind <- object@indicators
  if(length(ind) != 0){
    ind_names <- names(ind)
    max_length <- max(nchar(ind_names))
    ind_names <- str_pad(ind_names, width = max_length + 1, side = "right")
    ind_names <- paste0(ind_names, ": ")
  }
  aux <- list(lagMax = object@lagMax, verify = object@verify, parallel = object@parallel)
  aux_names <- names(aux)
  max_length_aux <- max(nchar(aux_names))
  aux_names <- str_pad(aux_names, width = max_length_aux + 1, side = "right")
  aux_names <- paste0(aux_names, ": ")


  " Class SM.dlm, method Show " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0(., "\n") %>%
    cat(.)

  cat("\n* Call: ")
  print(call)
  cat("\n")
  if(freq == 0){
    cat("* Frequecy of series : adjust automatic \n")
  }else if(freq == 1){
    cat("* Series without seasonality.\n")
  }else{
    cat("* Frequecy of series : ")
    cat(freq)
    cat("\n")
  }

  cat("\n")
  cat("* Index of event indicators : \n")

  if(length(ind) != 0){
    for(i in 1 : length(ind)){
      cat("    ")
      cat(ind_names[i])
      cat(ind[[i]])
      cat("\n")
    }

    cat("\n* Extra parameter: \n")
    for(i in 1 : length(aux)){
      cat("    ")
      cat(aux_names[i])
      cat(aux[[i]])
      cat("\n")
    }
  }else{
    cat("   No event has set & extra parameters has omitted.\n")
  }

  " SHOW INHERITED CLASS " %>%
    str_pad(., width = (50 - nchar(.))/2 + nchar(.), side = "left", pad = "-") %>%
    str_pad(., width = 50, side = "right", pad = "-") %>%
    str_pad(., width = 55, side = "left", pad = " ") %>%
    str_pad(., width = 60, side = "right", pad = " ") %>%
    paste0("\n", ., "\n\n") %>%
    cat(.)

  callNextMethod()

  " End Show(SM.dlm) " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0("\n", ., "\n") %>%
    cat(.)

  invisible()
}

.SM.dlm.fitted.show <- function(object){
  call <- object@call
  freq <- object@freq
  lags <- object@lags
  seasonalSign <- object@seasonalSign
  tracking <- object@tracking

  param <- sapply(object@parameters, round, 4)
  ind <- object@indicators

  param_names <- names(param)
  max_length <- max(nchar(param_names))
  param_names <- str_pad(param_names, width = max_length + 1, side = "right")
  param_names <- paste0(param_names, ": ")

  if(length(lags) != 0){
    lags_names <- names(lags)
    max_length <- max(nchar(lags_names))
    lags_names <- str_pad(lags_names, width = max_length + 1, side = "right")
    lags_names <- paste0(lags_names, ": ")
  }
  if(length(index) != 0){
    ind_names <- names(ind)
    max_length <- max(nchar(ind_names))
    ind_names <- str_pad(ind_names, width = max_length + 1, side = "right")
    ind_names <- paste0(ind_names, ": ")
  }


  " Class SM.dlm.fitted, method Show " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0(., "\n") %>%
    cat(.)

  cat("\n* Call: ")
  print(call)

  cat("\n* Best model parameters : ")
  cat("\n  ** Estimated parameters of dlm (with exp) : \n")
  for(i in 1 : length(param)){
    if(length(param[[i]]) > 1){
      cat("      ")
      cat(param_names[i])
      cat(exp(param[[i]]))
      cat("\n")
    }else{
      cat(paste0("      ", param_names[i], exp(param[[i]])), "\n")
    }
  }
  if(length(lags) != 0){
    cat("\n  ** Associated lag : \n")
    for(i in 1 : length(lags)){
      if(length(lags[[i]]) > 1){
        cat("      ")
        cat(lags_names[i])
        cat(lags[[i]])
        cat("\n")
      }else{
        cat(paste0("      ", lags_names[i], lags[[i]]), "\n")
      }
    }
  }else{
    cat("\n  ** No associated lag. \n")
  }
  cat("\n")
  if(freq == 0){
    cat("* Frequecy of series : adjust automatic \n")
  }else if(freq == 1){
    cat("* Series without seasonality.\n")
  }else{
    cat(paste0("* Frequecy of series : ", freq, "\n"))
    cat(paste0("  ** Seasonality significance :", seasonalSign, "\n"))
  }
  if(length(index) != 0){
    cat("\n* Index of event indicators : \n")
    for(i in 1 : length(ind)){
      if(length(ind[[i]]) > 1){
        cat("    ")
        cat(ind_names[i])
        cat(ind[[i]])
        cat("\n")
      }else{
        cat(paste0("    ", ind_names[i], ind[[i]]), "\n")
      }
    }
  }else{
    cat("\n* Index of event indicators no founded. \n")
  }
  cat("\n* Top 5 rows of tracking : \n\n")
  if(nrow(tracking) >= 5){
    print(tracking[1:5, ])
  }else{
    print(tracking)
  }
  cat("\n")

  " End Show(SM.dlm.fitted) " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0("\n", ., "\n") %>%
    cat(.)

  invisible()
}
.SM.HL.show <- function(object){
  call <- object@call
  offset <- object@offset

  " Class SM.HL, method Show " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0(., "\n") %>%
    cat(.)

  cat("\n* Call: ")
  print(call)
  cat("\n")
  cat(paste0("* Offset := ", offset, "\n"))

  " SHOW INHERITED CLASS " %>%
    str_pad(., width = (50 - nchar(.))/2 + nchar(.), side = "left", pad = "-") %>%
    str_pad(., width = 50, side = "right", pad = "-") %>%
    str_pad(., width = 55, side = "left", pad = " ") %>%
    str_pad(., width = 60, side = "right", pad = " ") %>%
    paste0("\n", ., "\n\n") %>%
    cat(.)

  callNextMethod()

  " End Show(SM.HL) " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0("\n", ., "\n") %>%
    cat(.)

  invisible()
}


.SM.rnn.show <- function(object){
  call <- object@call
  model_path <- object@model_path
  compile_options <- object@compile_options
  opt_hyperparameters <- object@optim_hyperparameters
  preprocess_parameter <- object@preprocess_parameter
  tracking <- object@tracking

  aux_names <- c("Location", "Compile Options", "Best Hyperparameters", "preprocess method", "lag") %>%
    str_pad(., width = max(nchar(.)) + 1, side = "right") %>%
    paste0("    ", ., ": ")

  names <- names(compile_options) %>%
    str_pad(., width = max(nchar(.)) + 1, side = "right") %>%
    paste0("            $", ., ": ")

  names_2 <- names(opt_hyperparameters) %>%
    str_pad(., width = max(nchar(.)) + 1, side = "right") %>%
    paste0("            $", ., ": ")

  " Class SM.rnn, method Show " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0(., "\n") %>%
    cat(.)

  cat("\n* Call: ")
  print(call)
  cat("\n")


  cat("\n* RNN model informations : \n")
  cat(paste0(aux_names[1], model_path, "\n"))
  cat(paste0(aux_names[2], "\n"))
  for(i in seq(length(compile_options))){
    cat(paste0(names[i], compile_options[i]))
    cat("\n")
  }
  cat(paste0(aux_names[3], "\n"))
  for(i in seq(length(opt_hyperparameters))){
    cat(paste0(names_2[i], opt_hyperparameters[i]))
    cat("\n")
  }
  cat(paste0(aux_names[4], preprocess_parameter$method, "\n"))
  cat(paste0(aux_names[5], preprocess_parameter$lag, "\n"))

  cat("\n* Top 5 rows of tracking : \n\n")
  if(nrow(tracking) >= 5){
    print(tracking[1:5, ])
  }else{
    print(tracking)
  }
  cat("\n")

  " End Show(SM.rnn) " %>%
    str_pad(., width = (60 - nchar(.))/2 + nchar(.), side = "left", pad = "*") %>%
    str_pad(., width = 60, side = "right", pad = "*") %>%
    paste0("\n", ., "\n") %>%
    cat(.)

  invisible()
}
