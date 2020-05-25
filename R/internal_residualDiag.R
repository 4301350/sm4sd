## Internal definition of residualDiag methods

# Unused internal function
{
# .SM.dlm.fitted.residualPlot <- function(object, resid){
#   filtered <- object@filtered
#   fitted <- filtered$f[ -1 ]
#
#   df <- data.frame(fitted, resid)
#   p <- ggplot(data = df, aes(x = fitted, y = resid)) +
#     geom_point() +
#     geom_smooth(method = "loess", se = F) +
#     labs(x = "Fitted values",
#          y = "Residuals",
#          title = "Residuals vs Fitted") +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.line = element_line(colour = "black"))
#
#   plot(p)
#   invisible()
# }
# .SM.dlm.fitted.scaleLocationPlot <-function(object, resid){
#   resi <- sqrt(abs(resid))
#   ylim <- c(0, max(resid, na.rm = TRUE))
#   yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = "Residuals")))
#
#   filtered <- object@filtered
#   fitted <- filtered$f[ -1 ]
#
#   df <- data.frame(fitted, resid)
#   p <- ggplot(data = df, aes(x = fitted, y = resid)) +
#     geom_point() +
#     geom_smooth(method = "loess", se = F) +
#     ylim(ylim[1], ylim[2]) +
#     labs(x = "Fitted values",
#          y = yl,
#          title = "Scale-Location Plot") +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.line = element_line(colour = "black"))
#
#   plot(p)
#   invisible()
# }
}

# get error
.SM.dlm.fitted.getResid <- function(object, type = c("standardized", "raw"), sd = TRUE){
  filtered <- object@filtered
  class(filtered) <- "dlmFiltered"

  if(type != "standardized" & type != "raw"){
    stop("No recognised type of error!")
  }else{
    resid <- residuals(filtered, type = type, sd = sd)
  }

  if( sd ){
    resid$res <- resid$res[-1]
    resid$sd <- resid$sd[-1]
  }else{
    resid <- resid[-1]
  }

  resid
}
# Confidence band
.SM.dlm.fitted.confBand <- function(df.res, gamma){
  ind <- resid <- sd <- NULL

  coef <- qnorm((1-gamma)/2 + gamma)
  ratio <- round(sum(df.res$resid >= -1 * coef * df.res$sd &
                       df.res$resid <= coef * df.res$sd)/nrow(df.res), 2)

  p1 <- ggplot(data = df.res, aes(x = ind, y = resid)) +
    geom_point() +
    geom_line(aes(y = sd * coef), col = "blue") +
    geom_line(aes(y = - 1 * sd * coef), col = "blue") +
    coord_cartesian(ylim = 1.5 * c(min(df.res$resid), max(df.res$resid))) +
    labs(title = paste0("Confidence Band Plot: ", gamma),
         ylabel = "Resid",
         caption = paste0(ratio * 100, "% inside of the band"), col = "red") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

  p1
}

.SM.dlm.fiited.chis2 <- function(df.res){
  ind <- norm_square <- ma_chi <- NULL

  ma <- function(x){
    aux <- x[1]
    for (i in seq(2, length(x))){
      aux <- c(aux, mean(x[1:i]))
    }
    aux
  }
  df.res$ma_chi <- ma(df.res$norm_square)

  p2 <- ggplot(data = df.res, aes(x = ind, y = norm_square)) +
    geom_point() +
    geom_line(aes(y = ma_chi), col = "red") +
    geom_line(aes(y = 1), col = "blue") +
    labs(title = "Chisq Test Plot",
         ylab = "Norm Error",
         caption = paste0("Mean of chisq estimator: ", round(mean(df.res$norm_square)))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    annotate("text", 1, 1, vjust = -1, label = "y = 1")

  p2
}

.SM.dlm.fitted.QQPlot <- function(df.res){
  resid <- sd <- NULL

  p3 <- ggplot(df.res, aes(sample = resid / sd)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Norm Q-Q plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

  p3
}

.SM.dlm.fitted.tests <- function(resid){
  # ad_test <- ad.test(resid) ## nortest package
  # jb_test <- jarque.bera.test(resid) ## tseries package
  y <- resid[2:length(resid)]
  x <- resid[1:(length(resid) -1 )]
  dw_test <- dwtest(y ~ x)

  # cat("  Normality test: residuals\n\n")
  # cat(paste0("    * ", ad_test$method, " :\n"))
  # cat(paste0(names(ad_test$statistic), " = ", round(ad_test$statistic, 4), ", p-value < ",
  #            ifelse( round(ad_test$p.value) == 0, "2.2e-16", ad_test$p.value), "\n\n"))
  #
  # cat(paste0("    * ", jb_test$method, " (skewness&kurtosis Test) :\n"))
  # cat(paste0(names(jb_test$statistic), " = ", round(jb_test$statistic, 4), ", ",
  #            names(jb_test$parameter), " = ", jb_test$parameter, ", p-value < ",
  #            ifelse(round(jb_test$p.value) == 0, "2.2e-16", jb_test$p.value), "\n\n"))

  cat(paste0("    Durbin-Watson Test : ",  dw_test$data.name, "\n\n"))
  cat(paste0(names(dw_test$statistic), " = ", round(dw_test$statistic, 4), ", p-value < ",
             ifelse(round(dw_test$p.value) == 0, "2.2e-16", dw_test$p.value), "\n"))
  cat(paste0("alternative hypotesis: ", dw_test$alternative, "\n"))

  invisible()
}

# Residual diagnosis
.SM.dlm.fitted.residualDiag <- function(object, gamma){

  res <- .SM.dlm.fitted.getResid(object, type = "raw", sd = TRUE)
  if(gamma > 1 & gamma < 0){
    stop("The significance level between 0 and 1.")
  }

  df.res <- data.frame(ind = seq(length(res$res)), resid = res$res, sd = res$sd, norm_square = (res$res / res$sd)**2)

  p1 <- .SM.dlm.fitted.confBand(df.res, gamma)
  p2 <- .SM.dlm.fiited.chis2(df.res)
  p3 <- .SM.dlm.fitted.QQPlot(df.res)

  options(warn = -1)
  grid.arrange(p1, p2, p3, ncol = 2)
  options(warn = 0)

  .SM.dlm.fitted.tests(df.res$resid)
  invisible()
}
