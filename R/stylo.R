


stylo = function(gui = TRUE,
             frequencies = NULL,
             parsed.corpus = NULL,
             features = NULL,
             path = NULL,
             metadata = NULL,
             filename.column = "filename",
             grouping.column = "author",
             corpus.dir = "corpus", ...) {



	# load the defaults by invoking a function to produce them, 
	# keep as a list
	default_params = stylo.default.settings()

	# load custom settings from a file (if exists), overwrite the defaults
	if(file.exists("stylo_config.txt") == TRUE) {
		saved_params = config_read("stylo_config.txt")
		default_params = utils::modifyList(default_params, saved_params)
	}


	# arguments directly passed by the user will be prioritized
	custom_params = list(...)
	config = utils::modifyList(default_params, custom_params)



	# optionally, launch GUI and overwrite any existing variable
	if (gui == TRUE) {
		# first, checking if the GUI can be displayed
		# (the conditional expression inspired by the generic function "menu")
		if (.stylo_gui_available()) {
			gui_params = gui.stylo()  # replace with a new GUI call
			config = utils::modifyList(config, gui_params)
		} else {
			message("")
			message("GUI could not be launched -- default settings will be used;")
			message("otherwise please pass your variables as command-line agruments\n")
		}
	}


	# here, saving the config file should be performed:
	#   -> so far, this is still performed by the engine function run_oppose() 
	#


	# calling the main function, with the final set of arguments
	results = do.call(run_stylo, config)

	return(results)
}

