

print.stylo.data <- 
function(x, ...) {

  if( length(attr(x,"description")) >0 ) {
    cat("\n")
    cat(rep("-",nchar(attr(x,"description"))),"\n",sep="")
    cat(attr(x,"description"),"\n")
    cat(rep("-",nchar(attr(x,"description"))),"\n",sep="")
    cat("\n")
  }

  if(is.matrix(x) == TRUE | is.data.frame(x) == TRUE ) {
    no.of.variables = length( x[1,] )
    no.of.samples = length( x[,1] )
    # let's limit the number of digits to be shown
    x = round(x,7)
      if(no.of.variables >11 ) {
        x = x[,1:11]
        x = cbind(x,rep("..."))
        colnames(x)[12] = " "
      }
      if(no.of.samples >10 ) {
        x = x[1:10,]
        x = rbind(x,rep("..."))
       rownames(x)[11] = " "
      }
    # to avoid printing recursively the object x, let's print its "part"
    print(x, quote=FALSE)
    cat("\n")
    cat("(total number of rows/columns:  ",no.of.samples, "/",
        no.of.variables, ")\n", sep="")
    cat("\n")  
  } else if(is.character(x) == TRUE) {
    no.of.elements = length(x)
      if(no.of.elements > 999) {
        x = x[1:999]
      }
    # first element's number (formatted to have at least 7 characters)
    cat(sprintf("%7s", "[1]  "))
    # first element
    cat(sprintf("%-15s", x[1]))
    # next elements
    for(i in 1:(length(x)-1) ) {
        # a newline and a number every 4th element
        if((i/4) == floor(i/4) ) { 
          cat("\n")
          cat(sprintf("%7s", paste("[",i+1,"]  ",sep="")))
        }
      # the subsequent elements of the vector
    cat(sprintf("%-15s", x[i+1] ))
    }
    cat("\n")
    cat("\n")
    cat("(total number of elements:  ",no.of.elements, ")\n", sep="")
    cat("\n")
    # objects that are not matrices/data frames  
  } else {
    cat(x)
    cat("\n")
    cat("\n")
  }

}
