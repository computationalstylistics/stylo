

summary.stylo.results <- function(object, ...) {

  cat("\n")
  cat("Function call:\n")
  cat(deparse(object$call), "\n")

  cat("\n")
  cat("Available variables:\n")
  cat("\n")

  # retrieving names and descriptions of existing variables
  x.descriptions = unlist(sapply(object, function(x) {attr(x,"description")} ))
  x.names = names(object)

  # accessing the existing variables' descriptions: the variables
  # "object$call" and "object$name" will be omitted
  for(line in 1: length(object) ) {
    if(x.names[line] != "call" & x.names[line] != "name") {
      cat( format(x.names[line],width=25,justify="right"),
                      "  ", attr( object[[line]],"description"), "\n", sep="")
    }
  }

  cat("\n")
  cat("These variables can be accessed by typing e.g.:\n")
  cat("\n")
  cat(format(paste(deparse(substitute(object)),"$",x.names[1],sep=""),
             width=25, justify="right"), "\n")

}

