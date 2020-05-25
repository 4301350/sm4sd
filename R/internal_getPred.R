## Internal definition of gerPred methods

.RS.AutoFore.getpredict <- function(object, name){
  name <- paste0(name, ".fore")
  fore <- object[name]
  class(fore) <- "forecast"
  as.data.frame(fore)
}
