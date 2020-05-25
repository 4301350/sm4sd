### imports, exports and global variables

# global variable   .  object index res offset conductivity groups

#' @import magrittr
# @importFrom forecast forecast accuracy
#' @importFrom stats ts residuals pchisq predict qnorm
#' @importFrom ggplot2 ggplot aes geom_point labs theme element_blank element_line geom_smooth
#'  theme_bw expand_limits stat_qq_line coord_cartesian scale_x_continuous scale_y_continuous
#'  annotate stat_qq geom_line ylim
#' @importFrom stringr str_pad
#' @importFrom generics compile
#' @importFrom methods callNextMethod new setOldClass
#' @importFrom dlm dlmModReg dlmMLE dlmSmooth dlmFilter dlmModSeas
#' @importFrom grDevices devAskNewPage dev.flush
#' @importFrom graphics par
# @importFrom nortest ad.test
#' @importFrom lmtest dwtest
# @importFrom tseries jarque.bera.test
#' @importFrom gridExtra grid.arrange
#' @importFrom timeSeries plot
#' @importFrom keras keras_model_sequential layer_dense layer_simple_rnn save_model_hdf5 load_model_hdf5
#'  compile
NULL

if(getRversion() >= "2.15.1") utils::globalVariables(c(".", "object", "index", "res", "offset",
                                                       "conductivity", "groups"))
devtools::build_manual(pkg = ".", path = NULL)
setOldClass("call")
setOldClass("character")
