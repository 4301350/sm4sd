.SM.predictRNN <- function(object, sigb, epsp){
  preprocess_param <- object["preprocess_parameter"]

  if( length(sigb) != length(epsp) ) stop("Data lengths must be equal.")

  X <- as.matrix(cbind(sigb, epsp))

  res <- .SM.buildRNN.preprocess(X = X, method = preprocess_param$method,
                                 pre_param_x = preprocess_param$X, pre_param_y = preprocess_param$Y,
                                 lag = preprocess_param$lag)
  new_model <- load_model_hdf5(object["model_path"])

  predict <- new_model %>%
    predict(res$X_data)

  predict
}
