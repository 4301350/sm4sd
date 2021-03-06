#' sm4sd: A package for computing the conductivity of the pore water of soil.
#'
#' The sm4sd provides several methods to extract the pore water electrical conductivity based on data
#'  including Hilhorst model (deterministic model) and time-varying dynamic linear model approach.
#'
#' Salt concentration of bulk is a strong indicator for world's agricultural productivity. One way to
#'  mesure it is through determining the pore water conductivity of soil, \ifelse{html}{\out{&#963<sub>p</sub>}}{\eqn{\sigma_p}{ASCII}}.
#'  According to \href{https://pdfs.semanticscholar.org/2ff3/70a503943086f073ee52f16b33c1b1ea5fc5.pdf}{Hilhorst}, \ifelse{html}{\out{&#963<sub>p</sub>}}{\eqn{\sigma_p}{ASCII}} can be determined from the equation:
#'  \ifelse{html}{\out{<div align="center"><MATH> &#963<sub>p</sub>=<span>&#949<sub>p</sub>&#963<sub>b</sub></span><span class = "bar">/</span><span>(&#949<sub>b</sub>-&#949<sub>&#963<sub>b</sub>=0</sub>)</span></MATH></div>}}{\deqn{\epsilon_p = \frac{\sigma_p*\epsilon_b}{(\sigma_b - \sigma_{\epsilon_b = 0})}}{ASCII}}
#'  where \itemize{
#'   \item \eqn{\epsilon} : electrical permittivity,
#'   \item \eqn{\sigma} : electrical conductivity,
#'   \item p : pore water,
#'   \item b : bulk soil.
#'  } The values \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}} and \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}} can be mesured
#'  in the bulk soil using a dielectric sensor; \ifelse{html}{\out{&#949<sub>p</sub>}}{\eqn{\epsilon_p}{ASCII}} is a function of temperature; and
#'  \ifelse{html}{\out{&#949<sub>&#963<sub>b</sub>=0</sub>}}{\eqn{\epsilon_{\sigma_b}=0}{ASCII}} appear as a offset of the linear relationship between
#'  \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}} and \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}} depending on soil type.
#'  In his work, he recommended using 4.1 as a generic offset.
#'
#'  Once the offset is fixed, this model is deterministic.  The \href{https://envcoglobal.com/files/docs/5te-manual-08.pdf}{producer of capacitance soil moisture sensors 5TE} recommends the use the
#'  an offset \ifelse{html}{\out{&#949<sub>&#963<sub>b</sub>=0</sub>}}{\eqn{\epsilon_{\sigma_b}=0}{ASCII}} of 6 while another study found that this value
#'  is appropriate and does not present a good linear relationship between \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}} and \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}} Giving the randomness,
#'  \href{https://www.researchgate.net/publication/329648035_Estimating_Pore_Water_Electrical_Conductivity_of_Sandy_Soil_from_Time_Domain_Reflectometry_Records_Using_a_Time-Varying_Dynamic_Linear_Model}{Basem, Jose and Gerd}
#'  described a statistical approach using the time-varying dynamic linear model (dlm). In this work, they considered the offset \ifelse{html}{\out{&#949<sub>&#963<sub>b</sub>=0</sub>}}{\eqn{\epsilon_{\sigma_b}=0}{ASCII}} as an unknow
#'  parameter which is also estimaded by the model. The model can be formulated as follow:
#'  \ifelse{html}{\out{<div align = "center"> <math>y<sub>t</sub> = A<sub>t</sub>x<sub>t</sub> + v<sub>t</sub> <math></div>}}{\deqn{y_t = A_tx_t+v_t}}
#'  \ifelse{html}{\out{<div align = "center"> <math>x<sub>t</sub> = x<sub><span>t-1</span></sub> + w<sub>t</sub> <math></div>}}{\deqn{x_t = x_{t-1} + w_t}}
#'  where \itemize{
#'   \item  \ifelse{html}{\out{y<sub>t</sub>}}{\eqn{y_t}} is an 1-dimensional vector, representing the observation at time t, \ifelse{html}{\out{&#949<sub>b</sub>}}{\eqn{\epsilon_b}{ASCII}};
#'   \item  \ifelse{html}{\out{x<sub>t</sub>}}{\eqn{x_t}} is an 2-dimensional uncorrelated vector, representing the state at time t, \ifelse{html}{\out{(x<sub>1</sub>, x<sub>2</sub>)<sub>t</sub>}}{\eqn{(x_1, x_2)_t}}, and
#'    \itemize{
#'     \item  \ifelse{html}{\out{x<sub>1</sub>}}{\eqn{x_1}} is the offset \ifelse{html}{\out{&#949<sub>&#963<sub>b</sub>=0</sub>}}{\eqn{\epsilon_{\sigma_b=0}}{ASCII}},
#'     \item  \ifelse{html}{\out{x<sub>2</sub>}}{\eqn{x_2}} is related to the product of \ifelse{html}{\out{&#963<sub>b</sub>, &#949<sub>p</sub>}}{\eqn{\sigma_b,\epsilon_p}{ASCII}};
#'    }
#'   \item  \ifelse{html}{\out{A<sub>t</sub>}}{\eqn{A_t}}, for our specific model, is 2-dimension vector \ifelse{html}{\out{(1, &#963<sub>b</sub> &#xd7 &#949<sub>p</sub>)<sub>t</sub>}}{\eqn{(1, \sigma_b \times \epsilon_p)_t}};
#'   \item  \ifelse{html}{\out{v<sub>t</sub>, w<sub>t</sub>}}{\eqn{v_t, w_t}} are white noise random variable with their own covariance matrix.
#'  }
#'
#' The sm4sd contrains several classes: \itemize{
#'  \item \code{\link{RS}}: raw signal class with 3 raw inputs, \ifelse{html}{\out{&#963<sub>b</sub>, &#949<sub>b</sub> and &#949<sub>p</sub>}}{\eqn{\sigma_b, \epsilon_b \mbox{ and } \epsilon_p}{ASCII}};
#'  \item \code{\link{SM-class}}: signal model class, it contains the \code{\link{RS}} and it's virtual, it means you can't create directly an instance with this class;
#'  \item \code{\link{SM.dlm}}: dynamic linear model for raw signal, it contains \code{\link{SM-class}} and the extra parameters for the model definition;
#'  \item \code{\link{SM.dlm.fitted}}: fitted dynamic linear model;
#'  \item \code{\link{SM.HL}}: Hilhorst model;
#'  \item \code{\link{SM.rnn}}: Recurrent Neural Network model for \ifelse{html}{\out{&#963<sub>b</sub>}}{\eqn{\sigma_b}{ASCII}} simulation.
#' } For more details, check the class definitions.
#'
#'
#' @docType package
#' @name sm4sd Package
NULL
