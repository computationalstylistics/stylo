

oppose = function(gui = TRUE,
             path = NULL,
             primary.corpus = NULL,
             secondary.corpus = NULL,
             test.corpus = NULL,
             primary.corpus.dir = "primary_set",
             secondary.corpus.dir = "secondary_set",
             test.corpus.dir = "test_set", ...) {



	# load the defaults by invoking a function to produce them, 
	# keep as a list
	default_params = stylo.default.settings()

	# load custom settings from a file (if exists), overwrite the defaults
	if(file.exists("oppose_config.txt") == TRUE) {
		saved_params = config_read("oppose_config.txt")
		default_params = utils::modifyList(default_params, saved_params)
	}


	# arguments directly passed by the user will be prioritized
	custom_params = list(...)
	config = utils::modifyList(default_params, custom_params)


	# adding the args passed directly by the user
	config$path = path
	config$primary.corpus = primary.corpus
	config$secondary.corpus = secondary.corpus
	config$test.corpus = test.corpus
	config$primary.corpus.dir = primary.corpus.dir
	config$secondary.corpus.dir = secondary.corpus.dir
	config$test.corpus.dir = test.corpus.dir


	# optionally, launch GUI and overwrite any existing variable
	if (gui == TRUE) {
		# first, checking if the GUI can be displayed
		# (the conditional expression inspired by the generic function "menu")
		if (.stylo_gui_available()) {
			gui_params = gui.oppose()  # replace with a new GUI call
			config = utils::modifyList(config, gui_params)
		} else {
			message("")
			message("GUI could not be launched -- default settings will be used;")
			message("otherwise please pass your variables as command-line agruments\n")
		}
	}


	# NOTE: here, saving the config file should be performed:
	#  -> so far, this is still performed by the engine function 
	#
	#


	# calling the main function, with the final set of arguments
	results = do.call(run_oppose, config)

	return(results)
}

