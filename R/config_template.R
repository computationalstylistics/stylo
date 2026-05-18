

config_template = function() {

	config_spec = list(

		input = list(
			title = "INPUT SETTINGS",
			description = "Settings related to corpus preprocessing.",

			fields = list(

				corpus.format = list(
					comment = "Please specify the format of your corpus files.",
					allowed = c("plain", "xml", "xml.drama", "xml.notitles", "html")
					),

				corpus.lang = list(
					comment = "What is the language of your corpus? This software provides
					a number of simple tokenizers, to split an input string of characters 
					into units conventionally referred to as 'words' or 'tokens'. This might
					be very much dependent on your language. Whenever unsure, plase choose
					a generic tokenizer ('Other') that should work for several languages 
					and scripts using Unicode. The generic tokenizer should work with Greek,
					Georgian, Cyryllic, Arabic, Persian, etc. More sofisticated word splitting
					is provided for the following languages: English, Polish, Latin, French, 
					German, Italian, Hungarian, Dutch, Spanish. A few variants are available, too:
					- 'English.contr': treats the contractions as single words, i.e. strings
					such as don't, you've etc. will not be split into two words.
					- 'English.all': keeps the contractions (as above), and also prevents
					from splitting compound words (mother-in-law, double-decker, etc.)
					- 'Latin.corr': since some editions do not distinguish the letters v/u,
					this option provides a consistent conversion to 'u' in each text."
					),

				encoding = list(
					comment = "Text encoding.",
					allowed = c("UTF-8", "native.enc")
					),

				use.existing.freq.tables = list(
					comment = "You can bypass the whole corpus upload and tokenization 
					by using your alreary existing table(s) of frequencies; it can be word 
					frequencies, chatecter n-grams, Part-of-Speech tags, syntax subrees, 
					or whatever features you want to extract from your input texts and pass them 
					to the classifier. It's totally up to you what you want to analyze,
					but please make sure that your dataset follows the expected formatting",
					allowed = c(TRUE, FALSE)
					),
	
				#frequencies = list(
				#	comment = "[Not entirely clear what is the relation between this variable 
				#	and the one immediately above; to be updated]"
				#	),

				use.existing.wordlist = list(
					comment = "You can use a predefined list of features (e.g. words) to be 
					analyzed. E.g., the major functions from the current library save the final 
					list of features into a dedicated output file named 'wordlist.txt' (unless you 
					specify a different name). You can, say, edit this file by either adding 
					any words to the list by deleting some (most?) of them; you cna also or mark 
					the unwanted words with '\\#' (just like these comments are marked). Switching 
					this option on prevents the script from overwriting the file, and makes sure that 
					the wordlist is loaded from there."
					),

				interactive.files = list(
					comment = "Otherwise, select files manually. [This is not fully implemented!]"
					),

				use.custom.list.of.files = list(
					comment = "This option makes it possible to upload the files using an external list
					of files. It should be named 'files_to_analyze.txt' and be put into the working
					directory. The items (i.g. file names) should be separated either by spaces,
					tabs, or newlines. The delimiters can be mixed and/or multiplied, thus even a messy 
					list should be interpreted correctly."
					)
				)
			),


		features = list(
			title = "FEATURE SETTINGS",
			description = "Feature extraction parameters.",

			fields = list(
				analyzed.features = list(
					comment = "In classical approaches, frequencies of the most frequent words (MFW)
					are used as the basis for multidimensional analyses. It has been argued, however, 
					that other features are also worth considering, especially word and/or 
					character n-grams. The general concept of n-grams is to divide a string 
					of single words/characters into a sequence of n elements. Allowed values:
					'w' for words, 'c' for characters"
					),

				ngram.size = list(
					comment = "Size of n-grams.",
					allowed = "any integer, although n>5 doesn't make much sense."
					),

				preserve.case = list(),

				mfw.min = list(
					comment = "How many MFW ('Most frequent Words') should be taken into analysis 
					(if mfw.min value = max.mfw, then no multiple iterations will be computed).",
					allowed = "integer, conventionally 100, but perhaps 1000 is a better choice."
					),

				mfw.max = list(),

				mfw.incr = list(),

				start.at = list(
					comment = "The 'start.at' option enables skipping top frequency words: 
					you should indicate the desired start position of your list (however, in most cases 
					you will probably prefer setting it to 1, the rank of the single most frequent word,
					so that no words are skipped at the top of the frequency spectrum)."
					),

				culling.min = list(
					comment = "Culling rate specifies the percentage of texts in a corpus 
					in which a given word must be found in order to be included in the analysis. 
					Thus, a 100% culling rate limits the analysis to words that appear at least once 
					in every text in the corpus; at a 50% culling rate, a word is included into 
					the analysis when it appears in at least half of the texts in the corpus; 
					a 0% culling rate (or no culling) means that no words are omitted.
					When min=max: see above."
					),

				culling.max = list(),

				culling.incr = list(),

				mfw.list.cutoff = list(
					comment = "Usually, it is recommended to cut off the tail of the word-list.
					However, if you do not want to cut the list (which might be _very_ welcome for topic
					modeling), then the variable may be set to an absurdly big number, or to 
					'mfw.list.cutoff = mfw.list.of.all' (and then you are advised to use a fast computer)."
					),

				stop.words = list(
					comment = "Optionally, specify a selection of words to be EXCLUDED from the analysis: 
					in computational linguistics, this is referred to as stop words. It is not very likely 
					to be increase classification performance in classical stylometry, though, since 
					the most frequent words are usually the very features you don't want to exclude.",
					allowed = "a vector of words, e.g. c('the', 'of', 'in', 'if'), default = NULL."
					),

				# Deleting pronouns (this is independent of the culling procedure).

				show.features = list(
					comment = "Some classifiers, e.g. Nearest Shrunken Centroids, allow for seeing 
					the features of the highest discriminative power. Say TRUE if you want to get them."
					)
				)
			),

		statistics = list(
			title = "STATISTICS SETTINGS",
			description = "Choice of method and its parameters.",

			fields = list(
				analysis.type = list(
					comment = "CA, MDS, PCR, PCV [tbd]"
					),

				distance.measure = list(
					comment = "Strictly speaking, the choice of the appropriate distance measure
					is the core of the statistical procedure provided by this software.
					(However, the distance measures do not apply to the PCA method). Although the choice 
					is not easy, some of the following measures seem to be more suitable for linguistic 
					purposes than others. On theoretical grounds, raw Euclidean Distance as well as raw 
					Manhattan Distance should be avoided in stylometry ('raw' means, in this context, that
					there is no feature scaling involved). Canberra Distance is somewhat troublesome 
					but effective e.g. for Latin (it should be combined with careful culling settings 
					and a limited number of MFW taken into analysis). For English, usually Classic Delta 
					is a good choice (Classic Delta is Manhattan Distance applied to z-scored frequencies).
					A recent paper by Evert et al. challenges this picture, though, by showing that Cosine Delta
					can be even better in some setups. A paper by Kestemont et al. provides evidence that
					the Min-Max Distance outperforms the competition. 
					#
					The available distance measures (choose ONE) are as follows:
					- 'delta' --> Classic Delta as introduced by Burrows
					- 'argamon' --> Argamon's Linear Delta (based on Euclidean principles)
					- 'eder' --> Eder's Delta (details: Eder 2022)
					- 'simple' --> all features scaled by simply applying y = sqrt(x)
					- 'manhattan' --> Manhattan Distance, no scaling (obvious and well documented)
					- 'canberra' --> Canberra Distance (simple, but sometimes surprisinly good)
					- 'euclidean' --> Euclidean Distance (the most _natural_ distance; no scaling)"
					),

				linkage = list(
					comment = "Method of building a dendrogram; applicable for cluster analysis and BCT",
					allowed = c("nj", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
					),

				consensus.strength = list(
					comment = "If the Bootstrap Consensus Tree was chosen, specify the strengh of the consensus
					that is needed to reproduce a given link in the final consensus tree."
					)
				)
			),


		sampling = list(
			title = "SAMPLING SETTINGS",
			description = "Tweak here if you want to split your input texts into multiple samples.",

			fields = list(
				sampling = list(
					comment = "Decide whether you want to sample from your input texts.",
					allowed = c("normal.sampling", "random.sampling", "no.sampling")
					),

				sample.size = list(
					comment = "When dealing with longer text, one might want to divide these into samples 
					of equal size. This can be achieved by setting the above variable to 'normal.sampling'
					and specifying the number of words per sample here.",
					allowed = "integer (expressed in words, even if you're using n-grams), default = 10000."
					),

				sampling.with.replacement = list(
					comment = "When the analyzed texts are significantly unequal in length, it is not a bad
					idea to prepare samples as randomly chosen 'bags of words'. To this end, set the sampling 
					variable to 'random.sampling;. The desired size of the sample should be indicated via 
					the 'sample.size' variable, and the number of samples via the 'number.of.samples' parameter.
					Sampling with and without replacement is also available (further reading: Eder 2015). 
					It is also possible to use the entire corpus texts as samples (regardless of their length 
					and differences therein). For this, set the sampling variable to 'no.sampling'."
					),

				sample.overlap = list(),

				number.of.samples = list()
			)
		),



		output_args = list(
			title = "OUTPUT SETTINGS",
			description = "Various options to make your final plot nice, but also a few other output parameters.",

			fields = list(
				display.on.screen = list(),

				write.pdf.file = list(
					comment = "Do you want to display the graph on the screen?"
					),

				write.jpg.file = list(
					comment = "Do you want to write the graph directly to a graphics file? Which format?
					You can display the graph on the screen AND write to a file (the latter will be done 
					with much better quality)."
					),
				
				write.png.file = list(),
				
				write.svg.file = list(),

				dump.samples = list(
					comment = "[not fully implemented yet, use on your own risk]"
					),

				plot.options.reset = list(
					comment = "[obsolete]"
					),

				plot.custom.height = list(
					comment = "Dimensions of the plot area (expressed in inches), font size,
					thickness of the lines used to plot the graphs."
					),

				plot.custom.width = list(),

				plot.font.size = list(),
				
				plot.line.thickness = list(),

				label.offset = list(
					comment = "Custom offset between labels and point."
					),

				add.to.margins = list(
					comment = "Custom margin size (in percentage of plot area)."
					),

				text.id.on.graphs = list(
					comment = "How to represent samples on PCA or MDS graphs.",
					allowed = c("labels", "points", "both")
					),

				colors.on.graphs = list(
					comment = "Do you want the graphs colored? The script will automatically assign 
					the same colors to those texts that have the same first segment of their 
					file names (the first string ending in '_').",
					allowed = c("colors", "greyscale", "black")
					),

				titles.on.graphs = list(
					comment = "Do you want titles on your graphs, showing the most important parameters?"
					),

				custom.graph.title = list(
					comment = "Do you want to set your custom (main) title on the final picture?
					If not specified, the working directory name will be used as the title"
					),

				dendrogram.layout.horizontal = list(
					comment = "Layout of dendrogram (Cluster Analysis only).",
					allowed = c("horizontal", "vertical")
					),

				pca.visual.flavour = list(
					comment = "Specify which PCA output you want to get: either classic PCA with PC1 and PC2 shown,
					or biplot with both the samples and the loadings, or a technical view with some more info.",
					allowed = c("classic", "loadings", "technical", "symbols")
					),

				save.distance.tables = list(
					comment = "You might want to save computed table(s) of distances for further analysis. 
					Switch the following option TRUE to make it possible. The same applies to the list 
					of words (or other features) actually used in analysis, i.e. after culling, pronoun deletion, 
					etc. Again, one might want to save the table of frequencies actually used."
					),

				save.analyzed.features = list(),

				save.analyzed.freqs = list()
			)
		),


		networks_args = list(
			title = "CONSENSUS NETWORKS SETTINGS",
			description = "Here you can specify the behavior of the Bootstrap Consensus Network analysis. 
			Please keep in mind, however, that network analysis is not available _just like this_ from inside 
			the 'stylo' package. Instead, it produces a table of edges (and, optionally, another table of nodes)
			that can be then exported to external network analysis tools such as Gephi.",

			fields = list(
				network = list(
					comment = "An output file (or files) will be generated when this option is set TRUE;
					if this is set FALSE, next options are immaterial."
					),

				network.tables = list(
					comment = "Output format: either one table (edges), or two (edges and nodes).",
					allowed = c("edges", "both")
					),

				network.type = list(
					comment = "When 'undirected' type of network is chosen (default), then the connections 
					'from' and 'to' are counted together (they are neither distinguished nor differentiated).
					When 'directed' network is chosen, then the incoming connections and the outcoming
					ones are counted separately.",
					allowed = c("directed", "undirected")
					),

				linked.neighbors = list(
					comment = "If this value is set to 1, then a link between a given sample and its nearest 
					neighbor is established; when it is set to 2, two neighbors are connected (the nearest neighbor 
					and the firs runner-up), etc. Default value is 3, which means that the nearest 
					neighbor and two runners-up are taken into consideration.",
					allowed = "integer"
					),

				edge.weights = list(
					comment = "The connections' weights are differentiated: the nearest neighbor has the strongest 
					link, then comes the first runner-up, and so forth. The difference between the assigned weights 
					might be linear = 1, 2, 3, ..., n;  quadratic = 1, 4, 9, ..., n^2; or 
					logarithmic = log(1+ (1, 2, 3, ..., n) ).",
					allowed = c("linear", "log", "quadratic")
				)
			)
		),



		various = list(
			title = "VARIOUS SETTINGS",
			description = "Whatever else needs to be decided.",

			fields = list(
				txm.compatibility.mode = list(
					comment = "This option enables integration with TXM corpus management system
					(see: Textometrie Project, http://textometrie.ens-lyon.fr/).
					Normally, it should be switched off, since it is used only when the script
					is invoked from inside the TXM environment. WARNING: experimental."
				),

				relative.frequencies = list(
					comment = "Choose either relative, or raw frequencies. In a vast majority of cases, 
					relative frequencies is the choice. But in topic modeling, raw numbers are to be used instead."
					)
				)
			),



		classify_args = list(
			title = "CLASSIFY SETTINGS",
			description = "Additional options used by the classify() function",

			fields = list(
				classification.method = list(
					comment = "Method of classification. Choose one of the options listed below:
					- 'delta' for the Delta method in its various flavors, 
					- 'knn' for k-nearest neighbor classification,
					- 'naivebayes' for Naive Bayes classification, 
					- 'nsc' for Nearest Shrunken Centroids,
					- 'svm' for Support Vectors Machines."
					),

				number.of.candidates = list(
					comment = "Delta is always active: output is directed to a file. You may specify
					the number of final ranking candidates to be displayded (at least 1).
					[maybe this option is not so relevant this days?]"
					),

				how.many.correct.attributions = list(
					comment = "Report the number of correct guesses for each iteration (written to 
					the log file)."
					),
			
				final.ranking.of.candidates = list(
					comment = "Ranking of the least unlikely candidates in the log file."
					),

				show.features = list(
					comment = "[works for NSC so far; there's a similar option above --> why?]"
					),

				svm.kernel = list(
					comment = "SVM settings (refer to help(svm) from library(e1071) for details).
					First, the kernel: choose between linear, polynomial or radial: 
					- 'linear' = u'*v 
					- 'polynomial' = (gamma*u'*v + coef0)^degree
					- 'radial' = exp(-gamma*|u-v|^2)"
					),

				svm.degree = list(
					comment = "degree: parameter needed for kernel of type 'polynomial' (default: 3)"
					),

				svm.coef0 = list(
					comment = "coef0: parameter needed for kernel of type 'polynomial' (default: 0)"
					),

				svm.cost = list(
					comment = "Cost of constraints violation (default: 1); it is the 'C'-constant 
					of the regularization term in the Lagrange formulation."
					),

				k.value = list(
					comment = "k value in k-NN, or number of neighbors considered:
					minimum vote for definite decision, otherwise 'doubt'. (More precisely, 
					less than 'k-l' dissenting votes are allowed, even if k is increased by ties)."
					),

				l.value = list(),

				z.scores.of.all.samples = list(
					comment = "How the z-scores should be calculated: if the variable is set to FALSE, 
					then the z-scores are relying on the primary set only (this should be better in most cases; 
					after all, this is the classical solution used by Burrows and Hoover). Otherwise, 
					the scaling is based on all the values in the primary and the secondary sets."
					),

				reference.wordlist.of.all.samples = list(
					comment = "Both talbes of frequencies (for the training set and the test set) are built 
					using the pre-prepared wordlist of the whole training set. Alternatively, one might want 
					to prepare this list from both sets."
					),

				culling.of.all.samples = list(
					comment = "Similarily with culling: it can be calcutated either on the training set alone, 
					or on both sets."
					),

				outputfile = list(
					comment = "File with the final ranking of classification results (log file)."
					),

				cv = list(
					comment = "Cross validation options: classify() only.
					['cv' is temporarily switched off, it always performs cv = 'stratified']",
					allowed = c("none", "standard", "stratified")
					),

				cv.folds = list(
					comment = "Cross validation folds."
					)
				)
			),



		oppose_args = list(
			title = "OPPOSE() SETTINGS",
			description = "Additional options used by the oppose() function",

			fields = list(
				text.slice.length = list(comment = "[to be integrated with the overall sampling options]"),

				text.slice.overlap = list(comment = "[to be integrated with the overall sampling options]"),

				rare.occurrences.threshold = list(
					comment = "If you want to ignore words that occurred only once or twice, set
					the threshold to 2; if you want to analyze all the words, set 0."
					),

				oppose.method = list(
					comment = "Method of calculating the Zeta scores.",
					allowed = c("craig.zeta", "eder.zeta", "chisquare.zeta", "mann.whitney", "box.plot")
					),

				zeta.filter.threshold = list(
					comment = "The meaning of the threshold parameter varies with the method chosen.
					If you picked 'craig.zeta', you might probably want to filter out some words
					of weak discrimination strength. Provided that 2 means the strongest 
					positive difference and O the strongest negative difference (Hoover 2009), 
					the values either just above or just below 1 are not significant and
					thus can be (or rather should be) omitted. If chisquare method was chosen,
					all the differences of p-value below 0.05 were filtered out, in pure Zeta,
					there is no a priori solution. Threshold 0.1 filters out a vast majority
					of words, threshold set to 1 filters all the words in a corpus."
					),

				plot.token = list(
					comment = "Initialize the token to be plotted if the 'box.plot' method is chosen
					[note to the developers: what about NULL?]."
					),

				visualization = list(
					comment = "[comment missing]",
					allowed = c("words", "markers", "none")
					),

				use.color.graphs = list(),
			
				polygons.on.graph = list(),

				identify.points = list(),

				classification = list(),

				naive.bayes = list(),

				svm.classification = list(),

				decision.tree.classification = list()
			)
		)
	)
	return(config_spec)
}


