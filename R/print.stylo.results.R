

print.stylo.results <- 
function(x, ...) {

  results.variable = c(
    "my.cool.results", "my.discovery", "my.stylometric.test",
    "blah.blah.blah", "seems.to.be.a.discovery", "publishable.results",
    "I.love.this.stuff", "x", "hip.hip.hurrah", "it.deserves.a.nobel.prize",
    "important.contribution", "my.boss.will.love.it", "best.results.ever")

  random.results.variable = sample(results.variable, 1)


  message("\n")
  message("Function call:\n")
  message(deparse(x$call), "\n")

  message("\n")
  message("Depending on your chosen options, some results should have been written\n")
  message("into a few files; you should be able to find them in your current\n")
  message("(working) directory. Usually, these include a list of words/features\n")
  message("used to build a table of frequencies, the table itself, a file containing\n")
  message("recent configuration, etc.\n")
  message("\n")
  message("Advanced users: you can pipe the results to a variable, e.g.:\n")
  message("\t",random.results.variable,"=", deparse(x$name),"\n")
  message("this will create a class", paste("\"",random.results.variable,"\"",sep=""), 
      "containing some presumably\n") 
  message("interesting stuff. The class created, you can type, e.g.:\n")
  message("\t summary(",random.results.variable,")\n")
  message("to see which variables are stored there and how to use them.\n")
  message("\n")
  message("\n")
  message("for suggestions how to cite this software, type: citation(\"stylo\")\n")
  message("\n")
  message("\n")

}

