# Internal function
## The purpose of this function is to make class mutable out of the class definition.

"@<-" <- function(object, name, value) {
  stop("For class consistency, modifying slot values are not allowed.")
}
