## Internal definition of extractMeasures methods

.extractMeasures.plot <- function(df){
  measures <- df
  p1 <- ggplot(measures, aes(x = seq(length(offset)), y = offset)) +
    geom_line() +
    labs(y = "offset",
         title = paste0("Offset = ", format(round(measures$offset[1], 2), nsmall = 2))) +
    coord_cartesian(ylim = c(0, 12)) +
    scale_x_continuous(breaks = seq(0, nrow(measures), 50)) +
    scale_y_continuous(breaks = seq(0, 12, 2)) +
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

  conduc <- measures$conductivity
  conduc <- conduc[is.finite(conduc)]
  dif <- (max(conduc) - min(conduc)) / 10
  y_aux <- min(conduc) + dif
  p2 <- ggplot(measures, aes(x = seq(length(conductivity)), y = conductivity)) +
    geom_line() +
    labs(x = "Time (min * 5)",
         y = "dS/m") +
    scale_x_continuous(breaks = seq(0, nrow(measures) ,50)) +
    annotate("text", x = nrow(measures) * 0.8, y = y_aux,
             label = expression("Pore water electrical\n", paste("conductivity (", sigma[p], ")"))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

  list(p1, p2)
}

.SM.dlm.extractMeasures <- function(object, plot){
  cat("In order to extract measures, the model must to be fitted.\n")
  stop("Fit the model first.")
}

.SM.dlm.fitted.extractMeasures <- function(object, plot){
  smoothed <- object@smoothed$s

  measures <- data.frame(offset = round(smoothed[, 1], 2),
                         conductivity = 1 / smoothed[, 2])

  plots <- .extractMeasures.plot(measures)
  p1 <- plots[[1]]
  p2 <- plots[[2]]

  if( plot ){
    options(warn = -1)
    grid.arrange(p1, p2, ncol = 1)
    options(warn = 0)
  }

  n <- nrow(measures)
  if( measures$offset[n] == mean(measures[round(n/2):n,]$offset) ){
    output <- list(mean(measures$offset), measures$conductivity)
  }else{
    output <- list(measures$offset, measures$conductivity)
  }
  names(output) <- names(measures)
  output
}
.SM.HL.extractMeasures <- function(object, plot){
  offset <- object@offset
  yt <- object@epsb
  xt <- object@epsp * object@sigb

  conductivity <- xt / (yt - offset)

  measures <- data.frame(offset = rep(offset, length(yt)),
                         conductivity = conductivity)

  plots <- .extractMeasures.plot(measures)

  p1 <- plots[[1]]
  p2 <- plots[[2]]
  if( plot ){
    options(warn = -1)
    grid.arrange(p1, p2, ncol = 1)
    options(warn = 0)
  }


  output <- list(mean(measures$offset), measures$conductivity)

  names(output) <- names(measures)
  output
}
