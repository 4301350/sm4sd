#' Predict Function for SM.rnn Class
#'
#' Simulation of \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}} based on the new
#'  \ifelse{html}{\out{&#949<sub>p</sub>}}{\eqn{\epsilon_p}{ASCII}} and
#'  \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}} according to the pattern captured by
#'  SM.rnn model.
#'
#'
#' @param object An SM.rnn class.
#' @param sigb,epsp A n-dimensional numeric vector of electrical conductivity of bulk soil measures and
#'   electrical permitivity of pore water measures.
#'
#' @section Note:
#' Before the prediction is applied, the data will be preprocessed according to the preprocessed method
#'   defined in the SM.rnn class.
#' @include internal_predict.R
#' @export RNN_predict
RNN_predict <- function(object, sigb, epsp){
  predict <- .SM.predictRNN(object, sigb, epsp)

  predict
}
