## Internal definition of plotCor methods

.RS.plot.corr <- function(x, groups, ...){
  group <- NULL

  yt <- x@epsb
  xt <- x@epsp * x@sigb

  if(groups %% 1 != 0 | groups <= 0){
    stop("Value of groups must to be a posive integer.")
  }
  if( groups > 6){
    groups <- 6
    warning("Group is exceeded 6, set to 6!")
  }

  df <- data.frame(xt = xt, yt= yt)
  if(groups == 1){
    p  <- ggplot(data = df, mapping = aes(x = yt, y = xt))
    p <-  p + geom_point(color = "blue") +
      labs(title = "epsilon-b vs. epsilon-p * sigma-b",
           x = "Epsilon-b",
           y = "Epsilon-p * Sigma-b") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))

    p
  }else{
    n <- length(yt)
    k <- ceiling(n / groups)
    df$group <- as.factor(rep(1 : groups, each = k)[1 : n])

    p  <- ggplot(data = df, mapping = aes(x = yt, y = xt, color = group, shape = group))
    p <- p + geom_point() +
      labs(title = "epsilon-b vs. epsilon-p * sigma-b",
           x = "Epsilon-b",
           y = "Epsilon-p * Sigma-b")

    p <- p + geom_smooth(method = "lm", se = F) +
      theme_bw() +
      expand_limits( y = 0 ) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))

    p
  }
}
