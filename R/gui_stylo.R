
#############################################################
#
# the GUI module for LANGUAGE
#
#############################################################

language_ui = function(id, config) {
	ns = NS(id)

	tagList(
		tags$h4("Input"),
		helpText("A few simple tokenizers are listed below. Pick 'Generic' for any language that is not listed. Please make sure your texts are in Unicode.", tags$a("(Read more)", href = "https://computationalstylistics.github.io/blog/stylo_and_languages/", target = "_blank")),

		selectInput(
			ns("corpus_lang"),
			label = "Language",
			choices = c(
				"Generic tokenizer" = "Other",
				"English" = "English",
				"English (contractions kept)" = "English.contr",
				"English (all kept)" = "English.all",
				"Latin" = "Latin",
				"Latin (u/v normalized)" = "Latin.corr",
				"Polish" = "Polish",
				"French" = "French",
				"German" = "German",
				"Hungarian" = "Hungarian",
				"Italian" = "Italian",
				"Dutch" = "Dutch",
				"Spanish" = "Spanish",
				"Chinese/Japanese/Korean" = "CJK"
				),
			selected = config$corpus.lang
			),
		checkboxInput(
			ns("encoding_native"),
			"Use native encoding (non UTF-8)",
			value = config$encoding != "UTF-8"
		)
	)
}


language_server = function(id) {
	moduleServer(id, function(input, output, session) {

		reactive({
			list(
			     corpus.lang = input$corpus_lang,

			     # convert back to original stylo convention
			     encoding = if (isTRUE(input$encoding_native)) {
				     "native.enc"
			     } else {
				     "UTF-8"
			     }
			)
		})
	})
}







#############################################################
#
# the GUI module for FEATURES
#
#############################################################

features_ui = function(id, config) {
	ns = NS(id)

	tagList(
		tags$h4("Features"),
		helpText("Different text units can be used as countable features. Here, please choose between words or characters. Word 1-grams splits the text into single words, and turns them into frequencies: this is the most intuivive, entry-level solution. This said, why not explorig other features, say character 3-grams?"),

		# --- Feature type ---
		radioButtons(
			ns("feat_type"), "Units",
			choices = c("Words" = "w", "Characters" = "c"),
			selected = config$analyzed.features,
			inline = TRUE
			),
		numericInput(
			ns("ngram_size"), "n-gram size:",
			value = config$ngram.size, min = 1
			),
		checkboxInput(
			ns("preserve_case"), "Preserve case",
			value = config$preserve.case
			),

		hr(),

		# --- MFW SETTINGS ---
		tags$h5("Most Frequent Words (MFW)"),

		fluidRow(
			 column(4,
				numericInput(ns("mfw_min"), "Min",
					value = config$mfw.min)
				),
			 column(4,
				numericInput(ns("mfw_max"), "Max",
					value = config$mfw.max)
				),
			 column(4,
				numericInput(ns("mfw_incr"), "Increment", 
					value = config$mfw.incr)
				)
			 ),

		hr(),

		# --- CULLING ---
		tags$h5("Culling"),

		fluidRow(
			 column(4,
				numericInput(ns("cull_min"), "Min",
					value = config$culling.min)
				),
			 column(4,
				numericInput(ns("cull_max"), "Max",
					value = config$culling.max)
				),
			 column(4,
				numericInput(ns("cull_incr"), "Increment",
					value = config$culling.incr)
				)
			 ),

		hr(),

		# --- VARIOUS ---
		tags$h5("Miscellaneous"),
		fluidRow(

			column(4,
			       numericInput(ns("start_at"), "Start at rank",
					value = config$start.at)
				),
			column(4,
				numericInput(ns("mfw_cutoff"), "List cutoff",
					value = config$mfw.list.cutoff)
			),
			column(3,
				checkboxInput(ns("delete_pronouns"), "Delete pronouns",
					value = config$delete.pronouns)
			)
		)
	)
}

features_server = function(id) {
	moduleServer(id, function(input, output, session) {

		reactive({
			list(
			     analyzed.features = input$feat_type,
			     ngram.size = input$ngram_size,
			     preserve.case = input$preserve_case,
			     mfw.min = input$mfw_min,
			     mfw.max = input$mfw_max,
			     mfw.incr = input$mfw_incr,
			     start.at = input$start_at,
			     culling.min = input$cull_min,
			     culling.max = input$cull_max,
			     culling.incr = input$cull_incr,
			     mfw.list.cutoff = input$mfw_cutoff,
			     delete.pronouns = input$delete_pronouns
			)
		})
	})
}






#############################################################
#
# the GUI module for STATISTICS
#
#############################################################

statistics_ui = function(id, config) {
	ns = NS(id)

	tagList(
		tags$h4("Statistics"),

		helpText("While lots of documentation on cluster analysis, PCA and MDS can be found online, both bootstrap consensus methods are introduced specifically to compare text similarities. See ", tags$a("here", href = "https://computationalstylistics.github.io/projects/bootstrap-networks/", target = "_blank"), "for a brief overview."),
		
		# --- Analysis type ---
		radioButtons(
			ns("analysis_type"), "Method",
			choices = c(
				"Hierarchical Cluster Analysis" = "CA",
				"Multidimensional Scaling (MDS)" = "MDS",
				"Principal Components Analysis (PCA)" = "PCA",
				"Bootstrap Consensus Tree (BCT)" = "BCT",
				"Bootstrap Consensus Network (BCN)" = "BCT"
				),
			selected = config$analysis.type
			),


		conditionalPanel(
			condition = sprintf("input['%s'] == 'PCA'",ns("analysis_type")),

			radioButtons(
				ns("pca_scaling"), "Scale the features",
				choices = c(
					"correlation table" = "PCR",
					"covariance table" = "PCV"
					),
				selected = config$analysis.type
				)
			),

		conditionalPanel(
			condition = sprintf("input['%s'] == 'BCT'",ns("analysis_type")),
			numericInput(
				ns("consensus_strength"),
				"Consensus strength",
				value = config$consensus.strength,
				min = 0, max = 1, step = 0.01
				)
			),

#		conditionalPanel(
#			condition = sprintf("input['%s'] == 'BCN'",ns("consensus_network")),
#			numericInput(
#				ns("consensus_strength"),
#				"Consensus strength",
#				value = config$consensus.strength,
#				min = 0, max = 1, step = 0.01
#				)
#			),

		hr(),

		# --- Distance ---
		tags$h5("Distance measure"),

		selectInput(
			ns("distance_measure"),
			label = "Distance",
			choices = c(
				"Classic Delta" = "delta",
				"Cosine Delta (Würzburg)" = "wurzburg",
				"Eder's Delta" = "eder",
				"Eder's Simple" = "simple",
				"Entropy" = "entropy",
				"Manhattan" = "manhattan",
				"Canberra" = "canberra",
				"Euclidean" = "euclidean",
				"Cosine" = "cosine",
				"Min-Max" = "minmax"
				),
			selected = config$distance.measure
		)
	)
}


statistics_server = function(id) {
	moduleServer(id, function(input, output, session) {

		reactive({
			list(
			     analysis.type = input$analysis_type,
			     consensus.strength = input$consensus_strength,
			     distance.measure = input$distance_measure
			)
		})
	})
}






#############################################################
#
# the GUI module for SAMPLING
#
#############################################################

sampling_ui = function(id, config) {
	ns = NS(id)

	tagList(
		tags$h4("Sampling"),

		radioButtons(
			ns("sampling_mode"), NULL,
			choices = c(
				"No sampling" = "no.sampling",
				"Normal samplng" = "normal.sampling",
				"Random sampling" = "random.sampling"),
			selected = config$sampling,
			inline = TRUE
			),

		# --- Sample size (normal + random) ---
		conditionalPanel(
			condition = sprintf(
				"input['%s'] == 'normal.sampling' || input['%s'] == 'random.sampling'",
				ns("sampling_mode"), ns("sampling_mode")
				),
			numericInput(
				ns("sample_size"),
				"Sample size",
				value = config$sample.size,
				min = 1
			)
		),

		# --- Number of samples (random only) ---
		conditionalPanel(
			condition = sprintf(
				"input['%s'] == 'random.sampling'",
				ns("sampling_mode")
				),
			numericInput(
				ns("num_samples"),
				"Number of random samples",
				value = config$number.of.samples,
				min = 1
			)
		)
	)
}


sampling_server = function(id) {
	moduleServer(id, function(input, output, session) {

		reactive({
			list(
			     sampling = input$sampling_mode,
			     sample.size = input$sample_size,
			     number.of.samples = input$num_samples
			)
		})
	})
}







output_ui = function(id, config) {
	ns = NS(id)

	tagList(
		tags$h4("Output"),

		# --- GLOBAL APPEARANCE ---
		tags$h5("Appearance"),

		radioButtons(
			ns("colors"), "Color mode:",
			choices = c(
				"Color" = "colors",
				"Grayscale" = "greyscale",
				"Black & White" = "black"
				),
			selected = config$colors.on.graphs,
			inline = TRUE
			),

		checkboxInput(
			ns("titles"),
			"Show title/subtitle on plots",
			value = config$titles.on.graphs
			),

		hr(),

		# --- EXPORT ---
		tags$h5("Export"),

		checkboxInput(
			ns("save_plot"),
			"Save plots to file",
			value = FALSE
			),

		conditionalPanel(
			condition = sprintf("input['%s']", 
					    ns("save_plot")),
			selectInput(
				ns("file_format"),
				"File format",
				choices = c("png", "jpg", "pdf", "svg"),
				selected = "png"
				),

			fluidRow(
				column(4,
				       numericInput(ns("plot_height"), "Height",
					value = config$plot.custom.height)
				       ),
				column(4,
				       numericInput(ns("plot_width"), "Width",
					value = config$plot.custom.width)
				       ),
				column(4,
				       numericInput(ns("font_size"), "Font size",
				value = config$plot.font.size)
					)
				),

				numericInput(
					ns("line_thickness"),
					"Line thickness",
					value = config$plot.line.thickness
				 )
				 ),

		hr(),

		# --- CONTENT ---
		tags$h5("Plot content"),

		radioButtons(
			ns("text_mode"), "Labels / points:",
			choices = c(
				"Labels" = "labels",
				"Points" = "points",
				"Both" = "both"
				),
			selected = config$text.id.on.graphs,
			inline = TRUE
			),
		fluidRow(
			column(6,
			       numericInput(ns("margins"), "Margins",
					value = config$add.to.margins)
			       ),
			column(6,
			       numericInput(ns("label_offset"), "Label offset",
				value = config$label.offset)
			)
			),

		tags$h5("PCA / MDS style"),

		radioButtons(
			ns("pca_flavour"), NULL,
			choices = c(
				"Classic" = "classic",
				"Loadings" = "loadings",
				"Technical" = "technical",
				"Symbols" = "symbols"
				),
			selected = NULL#config$pca.visual.flavour
			),

		hr(),

		# --- ADDITIONAL ---
		tags$h5("Additional options"),

		checkboxInput(
			ns("horizontal"),
			"Horizontal dendrogram",
			value = config$dendrogram.layout.horizontal
			),
		checkboxInput(
			ns("save_tables"),
			"Save distance tables",
			value = config$save.distance.tables
			),
		checkboxInput(
			ns("save_features"),
			"Save analyzed features",
			value = config$save.analyzed.features
			),
		checkboxInput(
			ns("save_freqs"),
			"Save frequency tables",
			value = config$save.analyzed.freqs
			),

		hr(),

		# --- RESET ---
		actionButton(
			ns("reset_output"),
			"Reset to defaults",
			class = "btn-warning"
		)
	)
}


output_server <- function(id, config) {
	moduleServer(id, function(input, output, session) {

		observeEvent(input$reset_output, {
			updateRadioButtons(session, "colors",
				selected = config$colors.on.graphs)
			updateCheckboxInput(session, "titles",
				value = config$titles.on.graphs)
			updateNumericInput(session, "plot_height",
				value = config$plot.custom.height)
			updateNumericInput(session, "plot_width",
				value = config$plot.custom.width)
			updateNumericInput(session, "font_size",
				value = config$plot.font.size)
			updateNumericInput(session, "line_thickness",
				value = config$plot.line.thickness)
			updateRadioButtons(session, "text_mode",
				selected = config$text.id.on.graphs)
			updateNumericInput(session, "margins",
				value = config$add.to.margins)
			updateNumericInput(session, "label_offset",
				value = config$label.offset)
			updateRadioButtons(session, "pca_flavour",
				selected = config$pca.visual.flavour)
			updateCheckboxInput(session, "horizontal",
				value = config$dendrogram.layout.horizontal)
			updateCheckboxInput(session, "save_tables",
				value = config$save.distance.tables)
			updateCheckboxInput(session, "save_features",
				value = config$save.analyzed.features)
			updateCheckboxInput(session, "save_freqs",
				value = config$save.analyzed.freqs)
		})

		reactive({

			# map format -> original flags
			write_flags <- list(
				write.pdf.file = FALSE,
				write.jpg.file = FALSE,
				write.png.file = FALSE,
				write.svg.file = FALSE
			)

			if (isTRUE(input$save_plot)) {
				fmt = input$file_format
				write_flags[[paste0("write.", fmt, ".file")]] <- TRUE
			}

			c(
			  list(
			       colors.on.graphs = input$colors,
			       titles.on.graphs = input$titles,
			       plot.custom.height = input$plot_height,
			       plot.custom.width = input$plot_width,
			       plot.font.size = input$font_size,
			       plot.line.thickness = input$line_thickness,
			       text.id.on.graphs = input$text_mode,
			       add.to.margins = input$margins,
			       label.offset = input$label_offset,
			       pca.visual.flavour = input$pca_flavour,
			       dendrogram.layout.horizontal = input$horizontal,
			       save.distance.tables = input$save_tables,
			       save.analyzed.features = input$save_features,
			       save.analyzed.freqs = input$save_freqs
			       ),
			  write_flags
			)
		})
	})
}












make_ui = function(config) {
	fluidPage(
		titlePanel("Stylo: conduct text analysis in style"),

		sidebarLayout(
			sidebarPanel(
				width = 4,
				helpText("You're welcome to push the 'Run' button at any time, but we encourage you to explore the options below. In some cases – perhaps in most cases – it is advisable to tweak the default options so that your settings are tailored to the corpus you are going to analyze."),

				actionButton("run_btn", "Run", class = "btn-success", width = "100%"),

				hr(),
				language_ui("lang_module", config),

				hr(),
				features_ui("feat_module", config),

				hr(),
				statistics_ui("stats_module", config),

				hr(),
				sampling_ui("sampling_module", config),

				hr(),
				output_ui("output_module", config)
				),

			mainPanel(
				width = 8,
				tabsetPanel(
					tabPanel("Visualization",
						plotOutput("result_plot", click = "plot_click")
						),
					tabPanel("Features",
						 verbatimTextOutput("raw_data_features"),
						 textOutput("raw_data_features_comment")
						 ),
					tabPanel("Frequencies",
						 renderTable("raw_data_frequencies"),
						 verbatimTextOutput("raw_data_frequencies"),
						 textOutput("raw_data_frequencies_comment")
						 ),
					tabPanel("Diagnostics",
						 verbatimTextOutput("debug_params")
					)
				)
			)
		)
	)
}



make_reporter = function(session = NULL) {

	if (!is.null(session)) {
		function(msg, type = "message") {
			showNotification(msg, type = type, duration = 3)
		}
	} else {
		function(msg, type = "message") {
			message(sprintf("[%s] %s", type, msg))
		}
	}
}






make_server = function(config_default_values) {
	function(input, output, session) {

		# modules
		lang_params = language_server("lang_module")
		feat_params = features_server("feat_module")
		stats_params = statistics_server("stats_module")
		sampling_params = sampling_server("sampling_module")
		output_params = output_server("output_module")

		# reporter
		reporter = make_reporter(session)

		# result storage
		computed_data = reactiveVal(NULL)

		observeEvent(input$run_btn, {
			reporter("Executing computation...")

			active_params = Reduce(
				modifyList, 
				list(
				     config_default_values,
				     lang_params(),
				     feat_params(),
				     stats_params(),
				     sampling_params(),
				     output_params()
				)
			)

			# inject reporter to use it later
			# active_params$reporter <- reporter

			results = tryCatch({
				do.call(run_stylo, active_params)
			}, error = function(e) {
				reporter(paste("Error:", e$message), type = "error")
				return(NULL)
			})

			computed_data(results)

			reporter("Computation finished.")
	})


	# populate the FEATURES tab on main screen
	output$raw_data_features = renderPrint({
		req(computed_data()$features)

		if(length(computed_data()$features) < 100) {
			print(computed_data()$features)	
		} else {
			print(computed_data()$features[1:100])
			output$raw_data_features_comment = renderText({
				"Showing the first 100 features."
			})	
		}	
	})


	# populate the FREQUENCIES tab on main screen
	output$raw_data_frequencies = renderPrint({
		req(computed_data())
		raw_data = computed_data()$table.with.all.freqs

		if(length(raw_data[1, ]) > 10) {
			raw_data = raw_data[, 1:10]
		}
		if(length(raw_data[, 1]) > 30) {
			raw_data = raw_data[1:30, ]
		}
		print(round(raw_data, 3))
		output$raw_data_frequencies_comment <- renderText({
			"Showing a fragment of the frequency table."
		})
	})


	# populate the DIAGNOSTICS tab on main screen
	output$debug_params = renderPrint({
		req(lang_params(), feat_params(), stats_params(),
		    sampling_params(), output_params()
		)
		
		diagnostic_params = c(
			lang_params(),
			feat_params(),
			stats_params(),
			sampling_params(),
			output_params()
		)
		str(diagnostic_params)
	})


	# populate the VISUALIZATION tab on main screen
	output$result_plot = renderPlot({
		req(computed_data())
		par(pty = "s")  # <- this one probably not needed
		plot(computed_data()$plot_obj)
	}, height = function() session$clientData$output_result_plot_width)

	#output$stats_text <- renderPrint({
	#	req(computed_data())
	#	summary(computed_data()$values)
	#})

	#output$raw_data_table <- renderTable({
	#	req(computed_data())
	#	computed_data()$values
	#})

	# --- testing: click interaction (future-ready) ---
	observeEvent(input$plot_click, {
		click = input$plot_click
		reporter(sprintf("Clicked at x=%.2f, y=%.2f", click$x, click$y))
		})
	}
}


