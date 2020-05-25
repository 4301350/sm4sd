#' Build RNN model
#'
#' \code{buildRNN} extends the class \code{\link{RS}} to \code{\link{SM.rnn}} by constructing and validating
#'  Keras models.
#'
#' @param object A RS class.
#' @param compile_options A list contaning the follow compile options: optimizer, loss and metrics. In case
#'  of missing ones of those indicators, default values are setted automatically:
#'  \itemize{
#'    \item{optimizer: }{adam}
#'    \item{loss: }{mean_absolute_percentage_error}
#'    \item{metrics: }{mean_absolute_percentage_error}
#'  }.
#'  For more detail, check \code{\link[keras:compile.keras.engine.training.Model]{compile}} function of Keras.
#'
#' @param hyperparameters A list contaning the follow parameters of Keras model:
#'   \itemize{
#'     \item{batch_size: }{	Integer or NULL. Number of samples per gradient update.
#'      If unspecified, batch_size will default to 32,}
#'     \item{epochs: }{Number of epochs to train the model. If unspecified, epochs will default to 10,}
#'     \item{test_split: }{the percentage of trained data that goes to testing. Default value 0.3,}
#'     \item{validation_split: }{the percentage of data that goes to validading model. Default value 0.3.}
#'   }
#'   Note: a vector input is acceptable for each hyperparameter, in this case gridsearch will be applied.
#'
#' @param preprocess_method A character value indicating data preparation method, available methos are
#'  \itemize{
#'    \item{normalized: }{normalization method using scale function,}
#'    \item{maxmin: }{maximum minimum transformation.}
#'  }
#' @param lag A integer value indicating how many previous data should be included in the data preparation
#'  process.
#' @param model_path A string value indicating where should the model to be stored.
#'
#' @section Detail:
#'  If lag is diferent a zero, the input data will be transformed to a new input with column number equal to
#'  2x(lag + 1), named n_col. Once the data preparetion is done including the preprocess method, a Keras
#'  sequential model is constructed with following layers:
#'   \itemize{
#'     \item{a simple layer with input shape equal to c(n_col, 1) and output shape equal to n_col,}
#'     \item{a simple rnn layer with n_col units,}
#'     \item{a simple layer with one unit with linear activation.}
#'   }
#'
#'   This model will be trained by first (1 - validation_split)\% rows (the last test_split part to test the
#'   model) and validated by the last validations_split\%. Once the model is done, it will be store to the
#'   model path because the S4 object can not treat the keras model as slot object.
#'
#' @examples
#' \dontrun{
#' n <- 50
#' # An example to create the RS class.
#'  my_first_class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                       epsb = rnorm(n, mean = 0, sd = 2),
#'                       epsp = rnorm(n, mean = 1, sd = 1))
#'  compile_options <- list()
#'  hyperparameters <- list(batch_size = c(16, 32, 64),
#'  epochs = c(10, 20, 50))
#'  preprocess_method <- "maxmin"
#'  lag <- 5
#'  model_path <- "~/Desktop/pans_model"
#'  myRNN <- buildRNN(object = myclass, compile_options = list(), hyperparameters = hyperparameters,
#'  preprocess_method = preprocess_method, lag = lag, model_path = model_path)
#' }
#'
#'
#' @name buildRNN
#' @docType methods
#' @rdname buildRNN-method
#' @include internal_buildRNN.R
#' @exportMethod buildRNN
setGeneric("buildRNN", function(object, compile_options, hyperparameters, preprocess_method,
                                lag, model_path) standardGeneric("buildRNN"))

#' @rdname buildRNN-method
setMethod(f = "buildRNN", signature = c("RS", "list", "list", "character",
                                        "numeric", "character"), definition = .SM.buildRNN)
