

# this function reads lines from a config file 
# and converts text strings into a list with variables

config_read = function(file) {

	if (file.exists(file) == FALSE) {	
		return(list()) # empty list if no config exists
	}

	tryCatch({

		# create a clean environment to source the file into
		# (this prevents the config file from polluting the workspace)
		config_env = new.env()
		sys.source(file, envir = config_env)

		# convert the environment entries into a list
		config_list = as.list(config_env)
	}, error = function(e) {
			warning("Failed to load config file: ", e$message)
			return(list())
		}
	)

	return(config_list)
}


