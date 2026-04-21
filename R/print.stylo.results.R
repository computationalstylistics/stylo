

print.stylo.results = function(x, ...) {
	results.variable = c(
		"my_cool_results", "my_discovery", "my_stylometric_test",
		"blah_blah_blah", "seems_to_be_a_discovery", "publishable_results",
		"I_love_this_stuff", "x", "xyz", "hip_hip_hurrah", 
		"it_deserves_a_nobel_prize", "look_at_this", "this_is_cool", 
		"hold_my_beer", "this_cannot_be_that_good",
		"important_contribution", "my_boss_will_love_it", 
		"best_results_ever")

	random.results.variable = sample(results.variable, 1)

	message("")
	message("Results produced by the function:")
	message(deparse(x$name))

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

