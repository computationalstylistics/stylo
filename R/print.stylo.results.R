

print.stylo.results <- 
function(x, ...) {

  results.variable = c(
    "my.cool.results", "my.discovery", "my.stylometric.test",
    "blah.blah.blah", "seems.to.be.a.discovery", "publishable.results",
    "I.love.this.stuff", "x", "hip.hip.hurrah", "it.deserves.a.nobel.prize",
    "important.contribution", "my.boss.will.love.it", "best.results.ever")

  random.results.variable = sample(results.variable, 1)


  cat("\n")
  cat("Function call:\n")
  cat(deparse(x$call), "\n")

  cat("\n")
  cat("Depending on your chosen options, some results should have been written\n")
  cat("into a few files; you should be able to find them in your current\n")
  cat("(working) directory. Usually, these include a list of words/features\n")
  cat("used to build a table of frequencies, the table itself, a file containing\n")
  cat("recent configuration, etc.\n")
  cat("\n")
  cat("Advanced users: you can pipe the results to a variable, e.g.:\n")
  cat("\t",random.results.variable,"=", deparse(x$name),"\n")
  cat("this will create a class", paste("\"",random.results.variable,"\"",sep=""), 
      "containing some presumably\n") 
  cat("interesting stuff. The class created, you can type, e.g.:\n")
  cat("\t summary(",random.results.variable,")\n", sep="")
  cat("to see which variables are stored there and how to use them.\n")
  cat("\n")
  cat("\n")
  cat("for suggestions how to cite this software, type: citation(\"stylo\")\n")
  cat("\n")
  cat("\n")

}

