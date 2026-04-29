
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

	# test whether GUI can be launched on this machine
	gui_exists = detect_gui_capability()

	# load custom settings from a file (if exists), overwrite the defaults
	if(file.exists("stylo_config.txt") == TRUE) {
		saved_params = config_read("stylo_config.txt")
		default_params = utils::modifyList(default_params, saved_params)
	}

	# arguments directly passed by the user will be prioritized
	custom_params = list(...)
	config = utils::modifyList(default_params, custom_params)

	# adding the args passed directly by the user
	config$frequencies = frequencies
	config$parsed.corpus = parsed.corpus
	config$features = features
	config$path = path
	config$metadata = metadata
	config$filename.column = filename.column
	config$grouping.column = grouping.column
	config$corpus.dir = corpus.dir


	# optionally, launch GUI and overwrite current config settings
	if (gui && gui_exists) {
		
		message(">>> Launching GUI...")

		# old good tcltk GUI starts here
		# switch to shiny GUI in due time
		#gui_params = gui.stylo()  
		#config = utils::modifyList(config, gui_params)

		# turn off direct plotting, let 'shiny' take over
		config$display.on.screen = FALSE

		shinyApp(
			ui = make_ui(config),
			server = make_server(config)
		)

	} else {

		message(">>> Running in CLI mode...")

		# switching to regular messages on screen
		reporter = make_reporter(NULL)

		# NOTE: here, saving the config file should be performed:
		#  -> so far, this is still performed by the engine function 
		# (or, maybe let's keep it where it was)
		#
		# calling the main function, with the final set of arguments
		results = do.call(run_stylo, config)
		return(results)

	}

}




