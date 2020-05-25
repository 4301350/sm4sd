# Internal function for buildRNN method.

.SM.buildRNN.parameterDefine <- function(compile_options, hyperparameters){
  # define default compile_options for missing data:
  default_compile <- c("optimizer", "loss", "metrics")
  default_compile_value <- c("adam", "mean_absolute_percentage_error", "mean_absolute_percentage_error")
  compile_names <- names(compile_options)

  optimizer_aux <- list()
  for(i in seq(length(default_compile))){
    if(!default_compile[i] %in% compile_names){
      optimizer_aux[[i]] <- default_compile_value[i]
    }else{
      n_index <- which(compile_names == default_compile[i])
      optimizer_aux[[i]] <- compile_options[[n_index]]
    }
  }
  names(optimizer_aux) <- default_compile

  # Define default hyperparameters for missing data:
  default_hyper <- c("batch_size", "epochs", "test_split", "validation_split")
  default_value <- c(32, 10, 0.3, 0.3)
  hyper_names <- names(hyperparameters)

  hyper_aux <- list()
  for(i in seq(length(default_hyper))){
    if(!default_hyper[i] %in% hyper_names){
      hyper_aux[[i]] <- default_value[i]
    }else{
      n_index <- which(hyper_names == default_hyper[i])
      hyper_aux[[i]] <- hyperparameters[[n_index]]
    }
  }
  names(hyper_aux) <- default_hyper

  res <- list(compile_options = optimizer_aux, hyperparameters = hyper_aux)
  res
}
.SM.buildRNN.normMethod <- function(X, Y, pre_param_x, pre_param_y){

  X <- sweep(X - rep(pre_param_x$center, each = nrow(X)), 2, pre_param_x$scale, FUN = "/")
  if( !is.null(Y) ){
    Y <- sweep(Y - rep(pre_param_y$center, each = nrow(Y)), 2, pre_param_y$scale, FUN = "/")
  }
  list(X = X, Y = Y)
}
.SM.buildRNN.maxminMethod <- function(X, Y, pre_param_x, pre_param_y){

  X <- sweep(X - rep(pre_param_x$min, each = nrow(X)), 2, pre_param_x$max - pre_param_x$min, FUN = "/")
  if( !is.null(Y) ){
    Y <- sweep(Y - rep(pre_param_y$min, each = nrow(Y)), 2, pre_param_y$max - pre_param_y$min, FUN = "/")
  }
  list(X = X, Y = Y)
}
.SM.buildRNN.preprocess <- function(X, Y = NULL, method, pre_param_x, pre_param_y, lag){

  if(method == "normalized"){
    res <- .SM.buildRNN.normMethod(X, Y, pre_param_x, pre_param_y)
  }
  if(method == "maxmin"){
    res <- .SM.buildRNN.normMethod(X, Y, pre_param_x, pre_param_y)
    res <- .SM.buildRNN.maxminMethod(res$X, res$Y, pre_param_x, pre_param_y)
  }

  if( lag != 0 ){
    aux_x <- res$X

    x1 <- matrix(aux_x[1 : (1 + lag), 1], ncol = lag + 1)
    x2 <- matrix(aux_x[1 : (1 + lag), 2], ncol = lag + 1)
    for(i in seq(2, nrow(aux_x) - lag)){
      x1 <- rbind(x1, aux_x[i : (i + lag), 1])
      x2 <- rbind(x2, aux_x[i : (i + lag), 2])
    }
    aux_x <- cbind(x1, x2)
    dim(aux_x) <- c(dim(aux_x), 1)

    if( !is.null(Y)){
      aux_y <- res$Y
      aux_y <- matrix(aux_y[(1 + lag) : nrow(aux_y)])
    }
  }

  if( !is.null(Y) ){
    list(X_data = aux_x, Y_data = aux_y)
  }else{
    list(X_data = aux_x)
  }
}
.SM.buildRNN.preprocessParam <- function(object, x1_name = "sigb", x2_name = "epsp",
                                         y_name = "epsb", method, lag = 0){
  X <- as.matrix(cbind(object[x1_name], object[x2_name]))
  Y <- as.matrix(object[y_name])

  if( method == "normalized" ){
    pre_param_x <- list(center = attr(scale(X), "scaled:center"),
                        scale = attr(scale(X), "scaled:scale"),
                        var_name = c(x1_name, x2_name))
    pre_param_y <- list(center = attr(scale(Y), "scaled:center"),
                        scale = attr(scale(Y), "scaled:scale"),
                        var_name = c(y_name))
  }else if( method == "maxmin"){
    pre_param_x <- list(center = attr(scale(X), "scaled:center"),
                        scale = attr(scale(X), "scaled:scale"),
                        max = apply(scale(X), 2, max) * 1.1 ,
                        min = apply(scale(X), 2, min) * 1.1,
                        var_name = c(x1_name, x2_name))
    pre_param_y <- list(center = attr(scale(Y), "scaled:center"),
                        scale = attr(scale(Y), "scaled:scale"),
                        max = apply(scale(Y), 2, max) * 1.1,
                        min = apply(scale(Y), 2, min) * 1.1,
                        var_name = c(y_name))
  }else{
    stop("The method is not valid.")
  }

  res <- .SM.buildRNN.preprocess(X = X, Y = Y, method = method,
                                 pre_param_x = pre_param_x, pre_param_y = pre_param_y,
                                 lag = lag)

  list(data = res,
       preprocess_parameter = list(X = pre_param_x, Y = pre_param_y, method = method, lag = lag))
}
.SM.buildRNN.split <- function(X, Y, split_rate, validation_rate){
  if(length(dim(X)) == 3){
    X <- X[, , 1]
  }

  if( is.matrix(X) ){
    n <- nrow(X)
    train_index <- seq(n * (1 - validation_rate))
    train_train <- seq(max(train_index) * (1 - split_rate))
    train_test <- train_index[-train_train]
  }

  X_train <- X[train_train, ]
  dim(X_train) <- c(dim(X_train), 1)

  X_test <- X[train_test, ]
  dim(X_test) <- c(dim(X_test), 1)

  X_validation <- X[-c(train_index), ]
  dim(X_validation) <- c(dim(X_validation), 1)

  list(X_train = X_train, X_test = X_test, X_valid = X_validation,
       Y_train = Y[train_train], Y_test = Y[train_test], Y_valid = Y[-train_index])
}
.SM.buildRNN <- function(object, compile_options, hyperparameters,
                         preprocess_method = c("normalized", "maxmin"),
                         lag,
                         model_path){

  call <- match.call()
  # redefine compile_options and hyperparameters including default value for missing info
  res <-.SM.buildRNN.parameterDefine(compile_options, hyperparameters)
  compile_aux <- res$compile_options
  hyper_aux <- res$hyperparameters

  # Define the preprocess parameters
  if( ! preprocess_method %in% c("maxmin", "normalized") ){
    stop("Method introduced has not defined for this function.")
  }
  res <- .SM.buildRNN.preprocessParam(object, x1_name = "sigb", x2_name = "epsp", y_name = "epsb",
                                      method = preprocess_method, lag = lag)
  X <- res$data$X_data
  Y <- res$data$Y_data
  preprocess_parameter <- res$preprocess_parameter


  param.grid <- expand.grid(batch_size = hyper_aux$batch_size,
                            epochs = hyper_aux$epochs,
                            test_split = hyper_aux$test_split,
                            validation_split = hyper_aux$validation_split,
                            metric = NA)
  if( compile_aux$metrics  == "mean_absolute_percentage_error"){
    names(param.grid)[5] <- "mape"
  }


  for( i in seq(nrow(param.grid)) ){

    split_data <- .SM.buildRNN.split(X, Y, param.grid$test_split[i], param.grid$validation_split[i])

    model <- keras_model_sequential()
    model %>%
      layer_dense(input_shape = dim(X)[2:3], units = dim(X)[2])
    model %>%
      layer_simple_rnn(units = dim(X)[2])
    # model %>%
    #   layer_simple_rnn(units = 2, return_sequences = T)
    model %>%
      layer_dense(units = 1, activation = "linear")

    model %>% compile(
      optimizer = compile_aux$optimizer,
      loss = compile_aux$loss,
      metrics = compile_aux$metrics
    )

    trained_model <- model %>% keras::fit(
      x = split_data$X_train,
      y = split_data$Y_train,
      batch_size = param.grid$batch_size[i],
      epochs = param.grid$epochs[i],
      validation_data = list(split_data$X_test, split_data$Y_test),
      shuffle = F
    )
    ypred <- model %>%
      predict(split_data$X_valid)

    param.grid[i, 5] <- sum(abs(ypred - split_data$Y_valid))

    model %>%
      save_model_hdf5(paste0("model_", i))
  }

  min.index <- which.min(param.grid[, 5])
  model_name <- paste0("model_", min.index)
  new_model <- load_model_hdf5(model_name)

  # aux <- new_model %>%
  #   predict(X)
  # aux_2 <- data.table::data.table(pred = aux[, 1], real = Y[, 1])
  # aux_2$dif <- abs(aux_2$pred - aux_2$real)
  files <- list.files()
  files <- files[grep("model_", files)]
  file.remove(files)

  new_model %>%
    save_model_hdf5(paste0(model_path))

  tracking <- param.grid[, c(1, 2, 5)]
  tracking <- tracking[order(tracking$mape), ]


  SM.rnn(call = call, model_path = model_path, compile_options = compile_aux,
         optim_hyperparameters = list(batch_size = param.grid$batch_size[min.index],
                                      epochs = param.grid$epochs[min.index]),
         preprocess_parameter = res$preprocess_parameter,
         tracking = tracking, object)
}
