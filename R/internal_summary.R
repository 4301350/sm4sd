# ## Internal definition of summary methods
# .RS.AutoFore.summary <- function(object, ...){
#   sigb.fore <- object@sigb.fore
#   epsb.fore <- object@epsb.fore
#   epsp.fore <- object@epsp.fore
#
#   class(sigb.fore) <- "forecast"
#   class(epsb.fore) <- "forecast"
#   class(epsp.fore) <- "forecast"
#
#   sigb.fore.m <- sigb.fore$model
#   epsb.fore.m <- epsb.fore$model
#   epsp.fore.m <- epsp.fore$model
#
#   cat("\nForecast model\n\n")
#   cat("Model Informations:\n")
#   cat(paste("    sigb: ", sigb.fore.m$method, "\n"))
#   cat(paste("    epsb: ", epsb.fore.m$method, "\n"))
#   cat(paste("    epsp: ", epsp.fore.m$method, "\n"))
#
#   cat("\n  Smoothing parameters:\n")
#   cat(paste("              ", "  sigb  ", "   epsb  ", "   epsp  ", "\n"))
#   cat(paste("    alpha = :   ",
#             format(round(sigb.fore.m$par["alpha"], 4), nsmall = 4), "  ",
#             format(round(epsb.fore.m$par["alpha"], 4), nsmall = 4), "  ",
#             format(round(epsp.fore.m$par["alpha"], 4), nsmall = 4), "\n"))
#   if(sigb.fore.m$components[2] != "N" | epsb.fore.m$components[2] != "N" | epsp.fore.m$components[2] != "N"){
#     cat(paste("    beta  = :   ",
#               format(round(sigb.fore.m$par["beta"], 4), nsmall = 4), "  ",
#               format(round(epsb.fore.m$par["beta"], 4), nsmall = 4), "  ",
#               format(round(epsp.fore.m$par["beta"], 4), nsmall = 4), "\n"))
#   }
#   if(sigb.fore.m$components[3] != "N" | epsb.fore.m$components[3] != "N" | epsp.fore.m$components[3] != "N"){
#     cat(paste("    gamma = :   ",
#               format(round(sigb.fore.m$par["gamma"], 4), nsmall = 4), "  ",
#               format(round(epsb.fore.m$par["gamma"], 4), nsmall = 4), "  ",
#               format(round(epsp.fore.m$par["gamma"], 4), nsmall = 4), "\n"))
#   }
#   if(sigb.fore.m$components[4] != "FALSE" | epsb.fore.m$components[4] != "FALSE" | epsp.fore.m$components[4] != "FALSE"){
#     cat(paste("    phi   = :   ",
#               format(round(sigb.fore.m$par["phi"], 4), nsmall = 4), "  ",
#               format(round(epsb.fore.m$par["phi"], 4), nsmall = 4), "  ",
#               format(round(epsp.fore.m$par["phi"], 4), nsmall = 4), "\n"))
#   }
#   cat("\n  Initial states:\n")
#   #cat(paste("         ", "  sigb  ", "   epsb  ", "   epsp  ", "\n"))
#   cat(paste("    l     = :   ",
#             format(round(sigb.fore.m$initstate[1], 4)), "  ",
#             format(round(epsb.fore.m$initstate[1], 4)), "  ",
#             format(round(epsp.fore.m$initstate[1], 4)), "\n"))
#   if(sigb.fore.m$components[2] != "N" | epsb.fore.m$components[2] != "N" | epsp.fore.m$components[2] != "N"){
#     cat(paste("    b     = :   ",
#               format(round(sigb.fore.m$initstate[2], 4)), "  ",
#               format(round(epsb.fore.m$initstate[2], 4)), "  ",
#               format(round(epsp.fore.m$initstate[2], 4)), "\n"))
#   }else{
#     sigb.fore.m$initstate <- c(sigb.fore.m$initstate[1], NA, sigb.fore.m$initstate[2:ncoef])
#     epsb.fore.m$initstate <- c(epsb.fore.m$initstate[1], NA, epsb.fore.m$initstate[2:ncoef])
#     epsp.fore.m$initstate <- c(epsp.fore.m$initstate[1], NA, epsp.fore.m$initstate[2:ncoef])
#     ncoef <- ncoef + 1
#   }
#   if(sigb.fore.m$components[3] != "N" | epsb.fore.m$components[3] != "N" | epsp.fore.m$components[3] != "N") {
#     cat("    s    = :  ")
#     if (ncoef <= 8) {
#       cat(paste(format(round(sigb.fore.m$initstate[3:ncoef], 4)), "  ",
#                 format(round(epsb.fore.m$initstate[3:ncoef], 4)), "  ",
#                 format(round(epsp.fore.m$initstate[3:ncoef], 4))))
#     }
#     else {
#       cat(paste(format(round(sigb.fore.m$initstate[3:8], 4)), "  ",
#                 format(round(epsb.fore.m$initstate[3:8], 4)), "  ",
#                 format(round(epsp.fore.m$initstate[3:8], 4))))
#       cat("\n           ")
#       cat(paste(format(round(sigb.fore.m$initstate[9:ncoef], 4)), "  ",
#                 format(round(epsb.fore.m$initstate[9:ncoef], 4)), "  ",
#                 format(round(epsp.fore.m$initstate[9:ncoef], 4))))
#     }
#     cat("\n")
#   }
#   #cat(paste("\n       ", "  sigb  ", "   epsb  ", "   epsp  "))
#   cat("\n  sigma :        ")
#   cat(paste(format(round(sqrt(sigb.fore.m$sigma2), 4)), "  ",
#             format(round(sqrt(epsb.fore.m$sigma2), 4)), "  ",
#             format(round(sqrt(epsp.fore.m$sigma2), 4))))
#   if (!is.null(sigb.fore.m$aic) | !is.null(epsb.fore.m$aic) | !is.null(epsp.fore.m$aic)) {
#     stats <- data.frame(matrix(ncol = 3))
#     stats[1, ] <- c(sigb.fore.m$aic, sigb.fore.m$aicc, sigb.fore.m$bic)
#     stats[2, ] <- c(epsb.fore.m$aic, epsb.fore.m$aicc, epsb.fore.m$bic)
#     stats[3, ] <- c(epsp.fore.m$aic, epsp.fore.m$aicc, epsp.fore.m$bic)
#     colnames(stats) <- c("AIC", "AICc", "BIC")
#     rownames(stats) <- c("sigb", "epsb", "epsp")
#     cat("\n\n")
#     print(stats)
#   }
#   cat("\nError measures for training set:\n\n")
#   acc <- data.frame(matrix(nrow = 3, ncol = 7))
#   acc[1, ] <- accuracy(sigb.fore)
#   acc[2, ] <- accuracy(epsb.fore)
#   acc[3, ] <- accuracy(epsp.fore)
#   row.names(acc) <- c("sigb", "epsb", "epsp")
#   colnames(acc) <- colnames(accuracy(sigb.fore))
#   print(acc, digits = 4)
#   if (is.null(sigb.fore$mean) & is.null(epsb.fore$mean) & is.null(epsp.fore$mean)) {
#     cat("\n No forecasts\n")
#   }
#   else {
#     cat("\nForecasts:\n")
#     cat(" - sigb \n")
#     print(as.data.frame(sigb.fore), digits = 4)
#     cat("\n - epsb\n")
#     print(as.data.frame(epsb.fore), digits = 4)
#     cat("\n - epsp\n")
#     print(as.data.frame(epsp.fore), digits = 4)
#   }
#
#   invisible()
# }
