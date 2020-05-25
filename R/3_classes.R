#' Class RS --- An S4 class to represent a raw signal inputs
#'
#' The RS class contains 3 equal length vector inputs: \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}},
#' \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}} and
#' \ifelse{html}{\out{&#949<sub>p</sub>}}{\eqn{\epsilon_p}{ASCII}}.
#'
#'
#' @slot sigb A n-dimensional numeric vector represents the electrical conductivity of bulk soil,
#'  \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}}.
#' @slot epsb A n-dimensional numeric vector represents the electrical permitivity of bulk soil,
#'  \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}}.
#' @slot epsp A n-dimensional numeric vector represents the electrical conductivity of pore water,
#'  \ifelse{html}{\out{&#949<sub>p</sub>}}{\eqn{\epsilon_p}{ASCII}}.
#'
#' @section Details:
#' Before the class is created, \code{\link[methods]{setValidity}} will be launched to check the length of 3
#'  input vectors.
#'  It will return "Error" if the lengths are not equal.
#'
#' @section Methods:
#' Available methods for this class are: \itemize{
#'  \item \code{\link{show}}: the class display is simplyfied, each slot is limited to 25;
#'  \item \code{\link{print}}: displays the whole class;
#'  \item \code{\link{get}} (`[`): returns the value of a slot;
#'  \item \code{\link{plot}}: plots 3 slots as "timeseries" objects in the same graphic;
#'  \item \code{\link{plotCor}}: plots the correlation between
#'   \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}}
#'   and \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}};
#'  \item \code{\link{buildClass}}: builds classes \code{\link{SM.dlm}} and \code{\link{SM.HL}};
#'  \item \code{\link{buildRNN}}: training a RNN model in order to build clase \code{\link{SM.rnn}}.
#' } Check the example below for more details.
#'
#' @examples
#' n <- 50
#' # An example to create the RS class.
#' my_first_class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                      epsb = rnorm(n, mean = 0, sd = 2),
#'                      epsp = rnorm(n, mean = 1, sd = 1))
#'
#' # Show the class
#' my_first_class
#' # print the class
#' print(my_first_class)
#' # Extract the slot "sigb"
#' sigb <- my_first_class["sigb"]
#' ### o  sigb <- `[`(my_first_class, "sigb)
#'
#' # plot the class as timeseries object
#' plot(my_first_class)
#' # plot the correlation
#' plotCor(my_first_class, 7)
#'
#' # build class
#' ## build dlm model
#' dlm_class <- buildClass(object = my_first_class,
#'                         method = "dlm", freq = 1,
#'                         ind = list(), lagMax = 3,
#'                         verify = TRUE, parallel = TRUE)
#' ## build HL model
#' mp_class <- buildClass(object = my_first_class,
#'                        method = "HL", ind = 9.5)
#' ### ind in this case is offset
#'
#' @exportClass RS
#' @export RS


## define the basic class: RawSignals
RS <- setClass(Class = "RS",
               ## class' variables
               slot =  list(
                 sigb = "numeric",
                 epsb = "numeric",
                 epsp = "numeric"
               ),
               ## prototype: initiation of the class
               prototype = prototype(
                 sigb = numeric(0),
                 epsb = numeric(0),
                 epsp = numeric(0)
               )
)

.RS.setvalidation <- function(object){
  n1 <- length(object@sigb)
  n2 <- length(object@epsb)
  n3 <- length(object@epsp)

  cat("--- Validating the RawSignal class ---\n")
  if( (n1 == n2) & (n1 == n3) ) {
    cat("OK\n")
    TRUE
  }else {
    cat("Inputs must have same length!\n")
    FALSE
  }
}
setValidity("RS", .RS.setvalidation)

#---------------------------------------------------------------------------------------------------------

# @noRd

# RS.AutoFore <- setClass(Class = "RS.AutoFore",
#                         slot = list(
#                           sigb.fore = "list",
#                           epsb.fore = "list",
#                           epsp.fore = "list"
#                         ),
#                         contains = c("RS")
# )

#---------------------------------------------------------------------------------------------------------

#' Class SM --- An virtual S4 class to represent the raw signal model.
#'
#' Class "SM" is a virtual super class and cornstone of specific model.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{sigb,epsb,epsp}}{Inherited slots from \code{\link{RS}}.}
#' }
#'
#' @section Details:
#' Available model extensions in this packages are: \itemize{
#'  \item \code{\link{SM.dlm}}: dynamic linear model, it's a space-state model to estimate the offset;
#'  \item \code{\link{SM.HL}}: Hilhorst model, it's a deterministic model where the offset is prefixed.
#' }
#'
#'
#' @section Extends:
#' From class \code{\link{RS}} directly without extra slots.


setClass(Class = "SM",
         contains = c("RS",
                      "VIRTUAL")
)

#---------------------------------------------------------------------------------------------------------

#' Class SM.dlm --- An S4 class to represent the unfiited dynamic linear model for raw signal
#'
#' SM.dlm class contrains the model defition based on the timeseries structure.
#'
#' @section objects from the Class:
#' Objects are created by the method \code{\link{buildClass}} or by calls of the form SM.dlm(...). However,
#' the first form is recommended.
#'
#' @section Slots:
#' \describe{
#'   \item{\code{call}}{An object of class \code{\link[base]{call}} returning an unevaluated function call.}
#'   \item{\code{freq}}{Positive integer value indicating the stationality of the raw inputs. Set it to 1 if the
#'         inputs has no stationality.}
#'   \item{\code{indicators}}{A list containing outliers or human intervention indexes, see examples.}
#'   \item{\code{lagMax}}{An integer value indicating the maximum effect lag for each outlier, see Details.}
#'   \item{\code{verify}}{A logical value indicating whether the validation process should be applied,
#'    see Details.}
#'   \item{\code{parallel}}{A logical value indicating whether the parallel process should be applied.}
#' }
#'
#' @section Details:
#'  Usually, outliers are happened in the data due to uncontrollable event or human intervention.
#'  In those cases, one should annotate the indexes where it presents to validate the model.
#'  check the example for more application detail.
#'
#'  Once outlier happended, its effect could be instant or remains some time period in case of timeseries. If
#'  lagMax is bigger than 1, then gridsearch will be applied: for each outlier index and for each possible
#'  lag between one and lagMax, the maximun likelihood estimation will be calculated for each possible
#'  combination. Then, best model will be returned after the gridsearch process which has the best MLE
#'  performance.
#'
#'  The number of models is determinated by follow equation: \deqn{number of models = lagMax ** number of indicators.}
#'  According to the formula, the gridsearch complexitity is an exponential
#'  function respect a #indicators with base lagMax. To avoid computation problem, it's best to assign verify
#'  equal to TRUE, where it returns FALSE if #indicators ** lagMax >= 200 in the class definition.
#'  To skip this validation process, kindly set verify to FALSE.
#'
#' @section Methods:
#' Available methods for this class are: \itemize{
#'  \item \code{\link{show}}: the class display is simplyfied, each data input is limited to 25;
#'  \item \code{\link{print}}: displays the whole class;
#'  \item \code{\link{get}} (`[`): returns the value of a slot;
#'  \item \code{\link{fit}}: training the model and returning the \code{\link{SM.dlm.fitted}}.
#'  \item \code{\link{extractMeasures}}: returning error, because the model hasn't trained.
#' }
#'
#' @section Extends:
#' From class \code{\link{SM-class}}, directly.
#'
#' @examples
#' n <- 50
#' rs.class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                epsb = rnorm(n, mean = 0, sd = 2),
#'                epsp = rnorm(n, mean = 1, sd = 1))
#' # The inputs we created are random variable,
#' # then they have non-stationality structure.
#' freq <- 1
#' # Suppose there is a rain at index 1 and 20, and irrigation at 5.
#' index <- list(rain = c(1, 20), watering = c(5))
#' lagMax <- 10
#' verify <- parallel <- TRUE
#' \dontrun{
#' # it returns error because 10 ** 3 = 1000 which is bigger than 200.
#' dlm.class <- buildClass(object = rs.class, method = "dlm",
#'                         freq = freq, ind = index, lagMax = lagMax,
#'                         verify = verify, parallel = parallel)
#' }
#' lagMax <- 3
#' dlm.class <- buildClass(object = rs.class, method = "dlm",
#'                         freq = freq, ind = index, lagMax = lagMax,
#'                         verify = verify, parallel = parallel)
#' dlm.class
#'
#' @exportClass SM.dlm
#' @export SM.dlm

SM.dlm <- setClass(Class = "SM.dlm",
                   slot = list(
                     call = "call",
                     freq = "numeric",
                     indicators = "list",
                     lagMax = "numeric",
                     verify = "logical",
                     parallel = "logical"
                   ),
                   contains = "SM"
)

.SM.dlm.setvalidation <- function(object){
  freq <- object@freq
  index <- object@indicators
  lagMax <- object@lagMax
  verify <- object@verify
  cat("--- Validating the SignalModel \'dlm\' class ---\n")
  if(freq < 1 | floor(freq) != freq ){
    stop("The frequency must be POSITIVE INTEGER.")
  }

  if(length(index) != 0){
    events <- unlist(index)
    events.round <- round(events)
    aux_count <- sum(events <= 0)
    aux_equal <- all.equal(events, events.round)

    if( aux_count != 0 | !aux_equal){
      stop("Event indicators must be POSITVE INTEGER.")
    }
    if( lagMax <= 0 | floor(lagMax) != lagMax){
      stop("Maxim lag must be a positive integer.")
    }
    if(lagMax ** length(events) > 200 & verify){
      stop("The number of model for gridsearch is excced to 200.")
    }
  }

  cat("OK\n")
  TRUE
}
setValidity("SM.dlm", .SM.dlm.setvalidation)

#---------------------------------------------------------------------------------------------------------

#' Class SM.dlm.fitted --- An S4 class to represent the fitted dynamic linear model
#'
#' The SM.dlm.fitted class store information about fitted dynamic linear model and its pre-definition.
#'
#' @section Objects from the Class:
#' SM.dlm.fitted is created by applying method \code{\link{fit}} to the \code{\link{SM.dlm}} class.
#'
#' @section Slots:
#'   \describe{
#     \item{\code{call}}{An object of class \code{\link[base]{call}} returning an unevaluated function call.}
#'     \item{\code{parameters}}{A list containing the best best parameters which are used to build the corresponded
#'       model by \code{\link{getMod}}.}
#'     \item{\code{filtered}}{A list contaning the filtered value. For more information, check the Value section of
#'       \code{\link[dlm]{dlmFilter}}.}
#'     \item{\code{smoothed}}{A list contaning the smoothed value. See also \code{\link[dlm]{dlmSmooth}}.}
#'     \item{\code{lags}}{A list contaning the best lag for each outlier indicator. For more detail, see the section
#'        Details of \code{\link{SM.dlm}}.}
#'     \item{\code{seasonalSign}}{A character indicating the significance of sesonal part in the model:\itemize{
#'         \item null: if the data doesn't have sesonal part (freq == 1)
#'         \item TRUE: the seasonality is significant in the model
#'         \item FALSE: the seasonality is not significant
#'         }. Check details for class building.}
#'     \item{\code{tracking}}{A data frame contaning the tracking of the gridsearch history ordered by the negative
#'       log likelihood.}
#'    }
#'
#' @section Methods:
#' Available methods for this class are: \itemize{
#'  \item \code{\link{show}}
#'  \item \code{\link{print}}
#'  \item \code{\link{get}} (`[`): giving the value of a slot
#'  \item \code{\link{getMod}}: returning the dlm model
#'  \item \code{\link{residualDiag}}: residual diagnostic for model validation
#'  \item \code{\link{extractMeasures}}: measurement extraction and visualization.
#' }
#'
#' @section Extends:
#'  From \code{\link{SM.dlm}}, directly.
#'
#' @examples
#' n <- 50
#' rs.class <- RS(sigb = rnorm(n, mean = 0, sd = 1),
#'                epsb = rnorm(n, mean = 0, sd = 2),
#'                epsp = rnorm(n, mean = 1, sd = 1))
#' # The inputs we created are random variable,
#' # then they have non-stationality structure.
#' freq <- 1
#' # Suppose there is a rain at index 1 and 20, and irrigation at 5.
#' index <- list(rain = c(1, 20), watering = c(5))
#' lagMax <- 10
#' verify <- parallel <- TRUE
#' lagMax <- 1
#'
#'  dlm.class <- buildClass(object = rs.class, method = "dlm",
#'                         freq = freq, ind = index, lagMax = lagMax,
#'                         verify = verify, parallel = parallel)
#'  dlm.fitted.class <- fit(dlm.class)
#'  dlm_model <- getMod(dlm.fitted.class)
#'
#' @exportClass SM.dlm.fitted
#' @export SM.dlm.fitted

SM.dlm.fitted <- setClass(Class = "SM.dlm.fitted",
                          slot = list(
                            # call = "call",
                            parameters = "list",
                            filtered = "list",
                            smoothed = "list",
                            lags = "list",
                            seasonalSign = "character",
                            tracking = "data.frame"
                          ),
                          contains = "SM.dlm"
)

#---------------------------------------------------------------------------------------------------------

#' Class SM.HL --- An S4 class to represent the Hilhorst model.
#'
#' @slot call An object of class \code{\link[base]{call}} returning an unevaluated function call.
#' @slot offset A no negative numeric value indicating
#'  \ifelse{html}{\out{&#949<sub>&#963<sub>b</sub>=0</sub>}}{\eqn{\epsilon_{\sigma_b}=0}{ASCII}}.
#'
#' @section Note:
#'  A hilhorst model is a deterministic model for this problem. For more detail, please check
#'   \code{\link{sm4sd Package}} and see also the paper of M.A.Hilhorst:
#'   \href{https://pdfs.semanticscholar.org/2ff3/70a503943086f073ee52f16b33c1b1ea5fc5.pdf}{A Pore Water Conductivity Sensor}.
#'
#' @exportClass SM.HL
#' @export SM.HL
SM.HL <- setClass(Class = "SM.HL",
                  slot = list(
                    call = "call",
                    offset = "numeric"
                  ),
                  contains = "SM"
)

.SM.HL.setvalidation <- function(object){

  offset <- object@offset
  cat("--- Validating the Magnus Persson class ---\n")
  if(  length(offset) != 1 | offset < 0 ){
    cat("he offset should be no negative length one numeric variable.")
    FALSE
  }else{
    cat("OK\n")
    TRUE
  }
}
setValidity("SM.HL", .SM.HL.setvalidation)

#---------------------------------------------------------------------------------------------------------

#' Class SM.rnn --- An S4 class contains a recurrent neural network model for
#'  \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}} simulation
#'
#' SM.rnn class contains the trained rnn model and its parameters.
#'
#' @section objects from the Class:
#'  Objects are created by the method \code{\link{buildRNN}}.
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{call}}{An object of class \code{\link[base]{call}} returning an unevaluated function call.}
#'    \item{\code{model_path}}{The directory where the trained rnn model should be stored.}
#'    \item{\code{compile_options}}{A list contanning compile options of keras model, check
#'     \code{\link{buildRNN}} for more detail.}
#'    \item{\code{optim_hyperparameters}}{A list contanning tunable hyperparameters, check
#'     \code{\link{buildRNN}}.}
#'    \item{\code{preprocess_parameter}}{A character value indicating data preparation method,
#'     \code{\link{buildRNN}}.}
#'    \item{\code{tracking}}{A data frame containing the tracking of the gridsearch history of keras model.}
#'  }
#'
#' @section Note:
#'  Check the Detail section of \code{\link{buildRNN}} to understand how SM.rnn model is created.
#'
#' @section Functions:
#' Available functions for this class are: \itemize{
#'  \item \code{\link{show}}: display the information of the RNN model
#'  \item \code{\link{RNN_predict}}: predict function.
#' }
#' @exportClass SM.rnn
#' @export SM.rnn

SM.rnn <- setClass(Class = "SM.rnn",
                   slot = list(
                     call = "call",
                     model_path = "character",
                     compile_options = "list",
                     optim_hyperparameters = "list",
                     preprocess_parameter = "list",
                     tracking = "data.frame"
                   ),
                   contains = "SM"
)


