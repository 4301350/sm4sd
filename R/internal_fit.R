## Internal definition of fit methods


# buildFunction used in dlm construction
.SM.dlm.buildFunction <- function(x, xt, yt, freq, index, lag_list) {
  if(freq == 0 | freq == 1){
    myMod <- dlmModReg(xt)
  }else{
    myMod <- dlmModReg(xt) + dlmModSeas(freq, dV = 0)
  }
  myMod$JFF[, 2] <- matrix(3)
  myMod$JV <- matrix(4)
  myMod$JW <- diag(c(1, 2))
  X1 <- matrix(rep(c(0, exp(x[2])), length(yt)), ncol = 2, byrow = T)
  X2 <- matrix(rep(exp(x[1]), length(yt)))
  myMod$X <- cbind(X1, myMod$X, X2)
  if(length(index) != 0){
    for(i in seq(length(index))){
      for(j in seq(length(index[[i]]))){
        j_init <- index[[i]][j]
        if(j + lag_list[[i]][j] - 1 > nrow(myMod$X)){
          j_final <- nrow(myMod$X)
        }else{
          j_final <- j_init + lag_list[[i]][j] - 1
        }
        myMod$X[j_init : j_final, 4] <- myMod$X[j_init : j_final, 4] +
          X2[j_init : j_final, 1] * exp(x[2 + i])
      }
    }
  }
  myMod
}
.SM.dlm.train <- function(object, i_call){
  epsb <- object@epsb
  epsp <- object@epsp
  sigb <- object@sigb
  freq <- object@freq
  index <- object@indicators
  lagMax <- object@lagMax

  # List length
  n <- length(index)
  # Total number of list elements
  n_total <- length(unlist(index))
  # Registre the length of each element of index
  n_index <- c()
  if(length(index) != 0 ){
    for(i in seq(length(index))){
      n_index <- c(n_index, rep(i, length(index[[i]])))
    }
  }
  # Define xt and yt
  yt <- epsb
  xt <- epsp * sigb

  if(!is.null(n_index)){
    # Define the grid for tunning parameter
    param_grid <- expand.grid(replicate(n_total, seq(lagMax), simplify = F))
    colnames(param_grid) <- names(unlist(index))

    # Define a dataframe storing parameter and MLE value.
    df <- data.frame()

    cat("Fitting")
    for(i in seq(nrow(param_grid))){
      lag_list <- split(as.numeric(param_grid[i,]), n_index)
      names(lag_list) <- names(index)

      build <- function(x){
        dlm <- .SM.dlm.buildFunction(x = x, xt = xt, yt = yt, freq = freq,
                                     index = index, lag_list = lag_list)
        dlm
      }

      # build dlm model
      param_aux <- rep(-1, length(index))
      tryCatch(
        {
          fit <- dlmMLE(yt, parm = c(-1, -1, param_aux), build = build)
          param <- c(as.integer(param_grid[i,]), fit$value, fit$par)
          names(param) <- c(names(param_grid), "LogLik", "V", "W", names(index))

          df <- rbind(df, as.data.frame(t(param)))
        }, error = function(e){
        }
      )
      cat(".")
    }
  }else{
    df <- data.frame()

    build <- function(x){
      dlm <- .SM.dlm.buildFunction(x = x, xt = xt, yt = yt, freq = freq,
                                   index = index, lag_list = lag_list)
      dlm
    }
    fit <- dlmMLE(yt, parm = c(-1, -1), build = build)
    param <- c(fit$value, fit$par)
    names(param) <- c(names(param_grid), "LogLik", "V", "W")

    df <- rbind(df, as.data.frame(t(param)))
    cat(".")
  }
  cat("\nDone.\n")
  min_logLik <- df[which.min(df$LogLik),]
  LogLik_pos <- which(names(min_logLik) == "LogLik")
  n_max <- ncol(min_logLik)
  param <- unlist(min_logLik[, (LogLik_pos + 1) : n_max])
  # names(param) <- colnames(min_logLik)[(LogLik_pos + 1) : n_max]
  if(length(index) != 0){
    lag_list <- split(unname(unlist(min_logLik[, 1: LogLik_pos - 1])), n_index)
    names(lag_list) <- names(index)
  }else{
    lag_list <- list()
  }
  dlmFit <- build(param)

  # extrac the parameter and their names
  param <- as.list(param)

  smoothed <- dlmSmooth(yt, dlmFit)
  filtered <- dlmFilter(yt, dlmFit)

  # Order the tracking list according the -LogLikelihood
  df <- df[order(df$LogLik), ]

  # Stationality test
  if( freq >= 2 ){
    cat("Stationality Test is applied: ")
    build_2 <- function(x){
      dlm <- .SM.dlm.buildFunction(x = x, xt = xt, yt = yt, freq = 1,
                                   index = index, lag_list = lag_list)
      dlm
    }
    fit_2 <- dlmMLE(yt, parm = c(-1, -1, param_aux), build = build_2)

    # Compute the -loglikelihood for two models
    log_1 <- df[1, ]$LogLik
    log_2 <- fit_2$value

    # Statistic of Wald test, it is assymptotically a chisq with 0 degree of freedoom.
    LR <- 2 * (log_2 - log_1)

    #
    chi.value <- pchisq(LR, 0, lower.tail = F)

    if(chi.value < 0.05){
      stationalitySign <- "TRUE"
      cat("The seasonality is significant.\n")
    }else{
      stationalitySign <- "FALSE"
      cat("The seasonality is not significant.\n")
    }
  }else{
    stationalitySign <- "NULL"
  }



  class(filtered) <- "list"
  class.fitted <- SM.dlm.fitted(call = i_call,
                                parameters = param,
                                filtered = filtered,
                                smoothed = smoothed,
                                lags = lag_list,
                                seasonalSign = stationalitySign,
                                tracking = df,
                                object)

  class.fitted
}
.SM.dlm.fit <- function(object){
  i_call <- match.call()
  class.fitted <- .SM.dlm.train(object, i_call)
  class.fitted
}
