

print.stylo.corpus <- 
function(x, ...) {

  cat("\n")
  no.of.samples = length(x)

  # reducing the number of samples to be shown:
  if(no.of.samples > 10) {
    max.samples = 10
  } else {
    max.samples = no.of.samples
  }

  for(i in 1:max.samples) {
    current.item.length = length(x[[i]])
      if(current.item.length >10) {
        current.item = x[[i]][1:10]
        } else {
        current.item = x[[i]][1:length(x[[1]])]
        }
    current.number = format(paste("[",1:10,"]",sep=""),width=6,justify="right")
    cat("sample", i, "\n")
    cat("\"",names(x)[i], "\"\n",sep="")
    cat( paste(current.number,current.item) , sep="\n")
      if(current.item.length > 10) {
      cat( format("...",width=6,justify="right"), "...", "\n")
      }
    cat("\n")
  }
  
  cat("(total number of samples:  ",no.of.samples, ")\n", sep="")
  cat("\n")
}
