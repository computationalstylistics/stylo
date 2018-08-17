

print.stylo.results = function(x, ...) {

  results.variable = c(
    "my.cool.results", "my.discovery", "my.stylometric.test",
    "blah.blah.blah", "seems.to.be.a.discovery", "publishable.results",
    "I.love.this.stuff", "x", "hip.hip.hurrah", "it.deserves.a.nobel.prize",
    "important.contribution", "my.boss.will.love.it", "best.results.ever")

  random.results.variable = sample(results.variable, 1)


  message("")
  message("Function call:")
  message(deparse(x$call))

  message("")
  message("Depending on your chosen options, some results should have been written")
  message("into a few files; you should be able to find them in your current")
  message("(working) directory. Usually, these include a list of words/features")
  message("used to build a table of frequencies, the table itself, a file containing")
  message("recent configuration, etc.")
  message("")
  message("Advanced users: you can pipe the results to a variable, e.g.:")
  message("\t ", random.results.variable, " = ", deparse(x$name))
  message("this will create a class ", paste("\"", random.results.variable, "\"", sep = ""), 
      " containing some presumably") 
  message("interesting stuff. The class created, you can type, e.g.:")
  message("\t summary(", random.results.variable, ")")
  message("to see which variables are stored there and how to use them.")
  message("")
  message("")
  message("for suggestions how to cite this software, type: citation(\"stylo\")")
  message("")
  message("")

}

