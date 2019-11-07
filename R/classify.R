
# Function that performs a number of machine-learning methods
# of classification used in computational stylistics: Delta (Burrows, 2002),
# k-Nearest Neighbors classification, Support Vectors Machines, Naive Bayes,
# and Nearest Shrunken Centroids (Jockers and Witten, 2010). Most of the options
# are derived from the 'stylo' function.

classify = function(gui = TRUE,
                    training.frequencies = NULL,
                    test.frequencies = NULL,
                    training.corpus = NULL,
                    test.corpus = NULL,
                    features = NULL,
                    path = NULL,
                    training.corpus.dir = "primary_set",
                    test.corpus.dir = "secondary_set", ...) {



# if any command-line arguments have been passed by a user, they will
# be stored on the following list and used to overwrite the defaults
passed.arguments = list(...)


# variable's initialization
cross.validation.summary = c()


# changing working directory (if applicable)
#
# first of all, retrieve the current path name
original.path = getwd()
# then check if anywone wants to change the working dir
if(is.character(path) == TRUE & length(path) > 0) {
  # checking if the desired file exists and if it is a directory
  if(file.exists(path) == TRUE & file.info(path)[2] == TRUE) {
    # if yes, then set the new working directory
    setwd(path)
  } else {
    # otherwise, stop the script
    stop("there is no directory ", getwd(), "/", path)
  }
} else {
  # if the argument was empty, then relax
  message("using current directory...")
}


if(is.character(training.corpus.dir)==FALSE | nchar(training.corpus.dir)==0) {
  training.corpus.dir = "primary_set"
}
if(is.character(test.corpus.dir) == FALSE | nchar(test.corpus.dir) == 0) {
  test.corpus.dir = "secondary_set"
}

# loading the default settings as defined in the following function
# (it absorbes the arguments passed from command-line)
variables = stylo.default.settings(...)




# optionally, displaying a GUI box
# (it absorbes the arguments passed from command-line)
if (gui == TRUE) {
      # first, checking if the GUI can be displayed
      # (the conditional expression is stolen form the generic function "menu")
      if (.Platform$OS.type == "windows" || .Platform$GUI ==
            "AQUA" || (capabilities("tcltk") && capabilities("X11") &&
            suppressWarnings(tcltk::.TkUp))) {
        variables = gui.classify(...)
      } else {
        message("")
        message("GUI could not be launched -- default settings will be used;")
        message("otherwise please pass your variables as command-line agruments\n")
      }
}




# #############################################################################
# Explicit assignment of all the variables, in order to avoid attach()
# #############################################################################

add.to.margins = variables$add.to.margins
analysis.type = variables$analysis.type
analyzed.features = variables$analyzed.features
classification.method = variables$classification.method
colors.on.graphs = variables$colors.on.graphs
consensus.strength = variables$consensus.strength
corpus.format = variables$corpus.format
corpus.lang = variables$corpus.lang
culling.incr = variables$culling.incr
culling.max = variables$culling.max
culling.min = variables$culling.min
culling.of.all.samples = variables$culling.of.all.samples
delete.pronouns = variables$delete.pronouns
dendrogram.layout.horizontal = variables$dendrogram.layout.horizontal
display.on.screen = variables$display.on.screen
distance.measure = variables$distance.measure
dump.samples = variables$dump.samples
final.ranking.of.candidates = variables$final.ranking.of.candidates
how.many.correct.attributions = variables$how.many.correct.attributions
interactive.files = variables$interactive.files
k.value = variables$k.value
l.value = variables$l.value
label.offset = variables$label.offset
linkage = variables$linkage
mfw.incr = variables$mfw.incr
mfw.list.cutoff = variables$mfw.list.cutoff
mfw.max = variables$mfw.max
mfw.min = variables$mfw.min
ngram.size = variables$ngram.size
number.of.candidates = variables$number.of.candidates
outputfile = variables$outputfile
passed.arguments = variables$passed.arguments
pca.visual.flavour = variables$pca.visual.flavour
plot.custom.height = variables$plot.custom.height
plot.custom.width = variables$plot.custom.width
plot.font.size = variables$plot.font.size
plot.line.thickness = variables$plot.line.thickness
plot.options.reset = variables$plot.options.reset
reference.wordlist.of.all.samples = variables$reference.wordlist.of.all.samples
sample.size = variables$sample.size
sampling = variables$sampling
sampling.with.replacement = variables$sampling.with.replacement
save.analyzed.features = variables$save.analyzed.features
save.analyzed.freqs = variables$save.analyzed.freqs
save.distance.tables = variables$save.distance.tables
start.at = variables$start.at
svm.coef0 = variables$svm.coef0
svm.cost = variables$svm.cost
svm.degree = variables$svm.degree
svm.kernel = variables$svm.kernel
text.id.on.graphs = variables$text.id.on.graphs
titles.on.graphs = variables$titles.on.graphs
txm.compatibility.mode = variables$txm.compatibility.mode
use.custom.list.of.files = variables$use.custom.list.of.files
use.existing.freq.tables = variables$use.existing.freq.tables
use.existing.wordlist = variables$use.existing.wordlist
write.jpg.file = variables$write.jpg.file
write.pdf.file = variables$write.pdf.file
write.png.file = variables$write.png.file
write.svg.file = variables$write.svg.file
z.scores.of.all.samples = variables$z.scores.of.all.samples
# #############################################################################



# newly-added options
relative.frequencies = variables$relative.frequencies
splitting.rule = variables$splitting.rule
preserve.case = variables$preserve.case
encoding = variables$encoding
cv.folds = variables$cv.folds
stop.words = variables$stop.words
sample.overlap = variables$sample.overlap
number.of.samples = variables$number.of.samples
custom.graph.title = variables$custom.graph.title

show.features = variables$show.features


# ['cv' is temporarily switched off, it always performs 'cv ="stratified"']
# cv = variables$cv






# #############################################################################
# Final settings (you are advised rather not to change them)
# #############################################################################


# Given a language option ("English", "Polish", "Latin" etc., as described
# above), this procedure selects one of the lists of pronouns
# If no language was chosen (or if a desired language is not supported, or if
# there was a spelling mistake), then the variable will be set to "English".

pronouns = stylo.pronouns(corpus.lang = corpus.lang)


# Since it it not so easy to perform, say, 17.9 iterations, or analyze
# 543.3 words, the code below justifies all numerical variables, to prevent
# you from your stupid jokes with funny settings. (OK, it is still
# possible to crash the script but we will not give you a hint)
  mfw.min = round(mfw.min)
  mfw.max = round(mfw.max)
  mfw.incr = round(mfw.incr)
  start.at = round(start.at)
  culling.min = round(culling.min)
  culling.max = round(culling.max)
  culling.incr = round(culling.incr)
  mfw.list.cutoff = round(mfw.list.cutoff)

# This also prevents from unexpected settings
if(number.of.candidates < 1) {
  number.of.candidates = 1
  number.of.candidates = round(number.of.candidates)
  }



###############################################################################
# Backward compatibility: if "use.existing.freq.tables" is switched on, then
# two files with frequency tables will be used, provided that they do exist
  if(use.existing.freq.tables == TRUE
                      & file.exists("freq_table_primary_set.txt") == TRUE
                      & file.exists("freq_table_secondary_set.txt") == TRUE ) {
    training.frequencies = "freq_table_primary_set.txt"
    test.frequencies = "freq_table_secondary_set.txt"
  } else {
    use.existing.freq.tables = FALSE
  }
# Backward compatibility: if "use.existing.wordlist" is switched on, then
# the file "wordlist.txt" will be used, provided that it does exist
  if(use.existing.wordlist == TRUE & file.exists("wordlist.txt") == TRUE ) {
    features = "wordlist.txt"
  } else {
    use.existing.wordlist = FALSE
  }
###############################################################################



# #############################################################################
# #############################################################################














# #################################################
# the module for loading a corpus from text files;
# it can be omitted if the frequency table for
# both primary and secondary sets already exist
# (then "use.existing.freq.tables" should be set
# to TRUE in the preamble of the script/GUI)
# #################################################
#

###############################################################################
# Checking if the argument "features" has been used (e.g. a custom wordlist)
#
# variable initialization:
features.exist = FALSE
#
  # Firstly, checking if the variable has at least two elements
  if(length(features) > 1) {
      # if yes, then checking whether it is a vector
      if(is.vector(features) == TRUE) {
        # if yes, then convert the above object into characters, just in case
        features = as.character(features)
        # link this vector into the variable used for calculations
        mfw.list.of.all = features
      } else {
        message("")
        message("You seem to have chosen an existing set of features")
        message("Unfortunately, something is wrong: check if your variable")
        message("has a form of vector\n")
        stop("Wrong format: a vector of features (e.g. words) was expected")
      }
    # selecting the above vector as a valid set of features
    features.exist = TRUE
  }
  # Secondly, checking if the variable has exactly one element;
  # presumably, this is a file name where a list of words is stored
  if(length(features) == 1) {
    # to prevent using non-letter characters (e.g. integers)
    features = as.character(features)
      # does the file exist?
      if(file.exists(features) == TRUE) {
        # file with a vector of features will be loaded
        message("\n", "reading a custom set of features from a file...")
        # reading a file: newlines are supposed to be delimiters
        features = scan(features, what = "char", sep = "\n", encoding = encoding)
        # getting rid of the lines beginning with the "#" char
        features = c(grep("^[^#]", features, value = TRUE))
        # link this vector into the variable used for calculations
        mfw.list.of.all = features
      } else {
        # if there's no such a file, then don't try to use it
        message("\n", "file \"", features, "\" could not be found")
        stop("Wrong file name")
      }
    # selecting the above vector as a valid set of features
    features.exist = TRUE
  }
###############################################################################







###############################################################################
# Checking if the argument "frequencies" has been used
#
# Iterating over two sets: the trainig set and the test set
for(iteration in 1:2) {
    # first iteration: training set
    if(iteration == 1) {
      frequencies = training.frequencies
    }
    # second iteration: test set
    if(iteration == 2) {
      frequencies = test.frequencies
    }


  # variable initialization:
  corpus.exists = FALSE

  # Firstly, checking if the variable has at least two elements
  if(length(frequencies) > 1) {
      # if yes, then checking whether it is a table or matrix
      if(is.matrix(frequencies) == TRUE | is.data.frame(frequencies) == TRUE) {
        # if yes, then convert the above object into a matrix (just in case)
        frequencies = as.matrix(frequencies)
      } else {
        message("")
        message("You seem to have chosen an existing table with frequencies")
        message("Unfortunately, something is wrong: check if your variable")
        message("has a form of matrix/data frame\n")
        stop("Wrong format of the table of frequencies")
      }
      # this code makes sure that the table has variables' names
      if(length(colnames(frequencies)) == 0) {
        colnames(frequencies) = paste("var", 1:length(frequencies[1,]), sep = "_")
      }
      # this code makes sure that the table has samples' names
      if(length(rownames(frequencies)) == 0) {
        rownames(frequencies) = paste("sample", 1:length(frequencies[,1]), sep = "_")
      }
    # selecting the above matrix as a valid corpus
    corpus.exists = TRUE
  }
  # Secondly, checking if the variable has exactly one element;
  # presumably, this is a file name where a table is stored
  if(length(frequencies) == 1) {
    # to prevent using non-letter characters (e.g. integers)
    frequencies = as.character(frequencies)
      # does the file exist?
      if(file.exists(frequencies) == TRUE) {
        # file with frequencies will be loaded
        message("\nreading a file containing frequencies...")
        frequencies = t(read.table(frequencies, encoding = encoding))
      } else {
        # if there's no such a file, then don't try to use it
        message("\n", "file \"", frequencies, "\" could not be found")
        stop("Wrong file name")
      }
    # selecting the above matrix as a valid corpus
    corpus.exists = TRUE
  }



  # If a custom set of features was indicated, try to pick the matching variables only
  if(features.exist == TRUE & corpus.exists == TRUE) {
      # checking if the chosen features do match the columns of the table
      if(length(grep("TRUE", colnames(frequencies) %in% features)) < 2) {
        message("The features you want to analyze do not match the variables' names:")
        message("")
        message("Available features: ", head(colnames(frequencies)), "...")
        message("Chosen features: ", head(features), "...")
        message("")
        message("Check the rotation of your table and the names of its rows and columns.")
        stop("Input data mismatch")
      } else {
        # if everything is right, select the subset of columns from the table:
        frequencies = frequencies[ , colnames(frequencies) %in% features]
      }
  }


  # If no custom features were chosen, take them from the variables' names
  if(features.exist == FALSE & corpus.exists == TRUE) {
     features = colnames(frequencies)
     # this is stupid, but this obsolete variable is needed somewhere (?)
     mfw.list.of.all = features
  }



  # Additionally, check if the table with frequencies is long enough
  if(corpus.exists == TRUE) {
    if(length(frequencies[,1]) < 2 | length(frequencies[1,]) < 2 ) {
      message("")
      message("There is not enough samples and/or features to be analyzed.")
      message("Try to use tables of at least two rows by two columns.\n")
      stop("Wrong size of the table of frequencies")
    }
  }

  # 1st iteration: setting the matrix containing the training set (if applicable)
  if(corpus.exists == TRUE & iteration == 1) {
    freq.I.set.0.culling = frequencies
    message("Training set successfully loaded.")
  }
  # 2nd iteration: setting the matrix containing the test set (if applicable)
  if(corpus.exists == TRUE & iteration == 2) {
    freq.II.set.0.culling = frequencies
    message("Test set successfully loaded.")
  }

# attempts at loading the training set and the test set: the loop returns here
}

# Two iterations completed, another sanity check should be applied
  # First, let's check if the I set was loaded
  if(!exists("freq.I.set.0.culling") & exists("freq.II.set.0.culling")) {
    message("Training set is missing, though.")
    message("Trying to build both tables from scratch.")
    corpus.exists = FALSE
  }
  # Secondly, let's check the II set
  if(exists("freq.I.set.0.culling") & !exists("freq.II.set.0.culling")) {
    message("Test set is missing, though.")
    message("Trying to build both tables from scratch.")
    corpus.exists = FALSE
  }
###############################################################################




# If the tables with frequencies could not loaded so far (for any reason),
# try to load an external corpus (R object) passed as an argument

###############################################################################
# Checking if the argument "training.corpus" and/or "test.corpus" has been used
#
# Iterating over two sets: trainig set and test set
for(iteration in 1:2) {
    # first iteration: training set
    if(iteration == 1) {
      parsed.corpus = training.corpus
    }
    # second iteration: test set
    if(iteration == 2) {
      parsed.corpus = test.corpus
    }

  # checking if the variable "parsed.corpus" is empty
  if(corpus.exists == FALSE & length(parsed.corpus) > 0) {
      # if the variable was used, check its format
      if(is.list(parsed.corpus) == TRUE & length(parsed.corpus) > 1) {
          # checking if the samples have their names; otherwise, assign generic ones:
          if( length(names(parsed.corpus)) != length(parsed.corpus) ) {
            names(parsed.corpus) = paste("sample", 1:length(parsed.corpus), sep="_")
          }
        # if everything is fine, use this variable as a valid corpus
#        loaded.corpus = parsed.corpus
      } else {
        message("")
        message("The object you've specified as your corpus cannot be used.")
        message("It should be a list containing particular text samples")
        message("(vectors containing sequencies of words/n-grams or other features).")
        message("The samples (elements of the list) should have their names.")
        message("Alternatively, try to build your corpus from text files (default).\n")
        stop("Wrong corpus format")
      }
  }

  # 1st iteration: setting the matrix containing the training set (if applicable)
  if(iteration == 1) {
    corpus.of.primary.set = parsed.corpus
  }
  # 2nd iteration: setting the matrix containing the test set (if applicable)
  if(iteration == 2) {
    corpus.of.secondary.set = parsed.corpus
  }
# attempts at loading the training set and the test set: the loop returns here
}

# Two iterations completed, another sanity check should be applied
if(corpus.exists == FALSE) {
    if(length(corpus.of.primary.set) >1 & length(corpus.of.secondary.set) >1 ) {
      message("Two subcorpora loaded successfully.")
      corpus.exists = TRUE
    } else {
      message("The subcorpora will be loaded from text files...")
      corpus.exists = FALSE
    }
}
###############################################################################




# If there's still no corpus available, then load and parse text files.
# They are supposed to be stored in a specified corpus subfolder and to follow
# a strictly defined naming convention.

###############################################################################
# Building subcorpora from text files

if(corpus.exists == FALSE) {

  # Checking whether required files and subdirectories exist
  # First check: allow user to choose a suitable folder via GUI
  if(file.exists(training.corpus.dir) == FALSE | file.exists(test.corpus.dir) == FALSE) {
    selected.path = tk_choose.dir(caption = "Select your working directory. It should two subdirectories called *primary_set* and *secondary_set*")
    setwd(selected.path)
  }
  
  # If the user failed to provide a suitable folder at this point, abort.
  if(file.exists(training.corpus.dir) == FALSE | file.exists(test.corpus.dir) == FALSE) {
    message("\n\n", "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
            "Working directory should contain two subdirectories:
        \"", training.corpus.dir, "\" and \"", test.corpus.dir, "\"\n",
            "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n", sep = "")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }
  
  # Retrieving the names of samples
  #
  filenames.primary.set = list.files(training.corpus.dir)
  filenames.secondary.set = list.files(test.corpus.dir)
  
  # Checking if the subdirectories contain any stuff
  if(length(filenames.primary.set) <2 | length(filenames.secondary.set) <2) {
    message("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
            "Both subdirectories \"", training.corpus.dir, "\" and \"",
            test.corpus.dir, "\"\nshould contain at least two text samples!\n",
            "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }

  # loading text files, splitting, parsing, n-gramming, samping, and so forth
  corpus.of.primary.set = load.corpus.and.parse(files = filenames.primary.set,
                         corpus.dir = training.corpus.dir,
                         encoding = encoding,
                         markup.type = corpus.format,
                         corpus.lang = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         sample.overlap = sample.overlap,
                         number.of.samples = number.of.samples,
                         features = analyzed.features,
                         ngram.size = ngram.size)

  # loading text files: test set
  corpus.of.secondary.set = load.corpus.and.parse(files = filenames.secondary.set,
                         corpus.dir = test.corpus.dir,
                         encoding = encoding,
                         markup.type = corpus.format,
                         corpus.lang = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         sample.overlap = sample.overlap,
                         number.of.samples = number.of.samples,
                         features = analyzed.features,
                         ngram.size = ngram.size)
}
###############################################################################




# At this point, some corpora SHOULD be available. If there's still no frequency
# tables, they will be build at this stage

###############################################################################
# building tables of frequencies

if(!exists("freq.I.set.0.culling") | !exists("freq.II.set.0.culling")) {

  # blank line on the screen
  message("")

  # both corpora (training set and test set) shoud contain some texts;
  # if the number of text samples is lower than 2, the script will stop
  if(length(corpus.of.primary.set) < 2 || length(corpus.of.secondary.set) < 1) {
    message("\n\n", "either the training set or the test set is empty!", "\n")
    stop("corpus error")
  }

  # If an external vector of features (usually: the most frequent words) has not
  # been specified (cf. the argument "features"), then we need a list of the most
  # frequent words (or n-grams, or anything else) used in the current corpus,
  # in descending order, without frequencies (just a list of words/features).
  if (features.exist == TRUE) {
    message("")
    message("using an existing wordlist (vector of features)...")
    mfw.list.of.all = features
  } else {
    # Extracting all the words (features) used in the texts of primary set
    # (or both if "Z-scores all" is set to TRUE)
    wordlist.of.primary.set = c()
    message("")
    # iterating over the samples stored in corpus.of.primary.set
    for (file in 1 : length(corpus.of.primary.set)) {
      # loading the next sample from the list filenames.primary.set,
      current.text = corpus.of.primary.set[[file]]
      # putting the samples together:
      wordlist.of.primary.set = c(wordlist.of.primary.set, current.text)
      # short message on screen
      message(".", appendLF = FALSE)
      if(file/25 == floor(file/25)) { message("")} # a newline every 25th sample
    }
    # including words of the secondary set in the reference wordlist (if specified)
      if (reference.wordlist.of.all.samples == TRUE) {
        wordlist.of.secondary.set = c()
        message("")
        for (file in 1 : length(corpus.of.secondary.set)) {
          # loading the next sample from the list filenames.secondary.set,
          current.text = corpus.of.secondary.set[[file]]
          # putting samples together:
          wordlist.of.secondary.set = c(wordlist.of.secondary.set, current.text)
          # short message on screen
          message(".", appendLF = FALSE)
          if(file/25 == floor(file/25)) { message("")} # a newline every 25th sample
        }
      } else {
        # otherwise, create an empty vector
        wordlist.of.secondary.set = c()
      }

    # Preparing a sorted frequency list of the whole primary set (or both sets).
    # short message
    message(" ")
    message(length(c(wordlist.of.primary.set, wordlist.of.secondary.set)), " tokens ",
         "will be used to create a list of features")
    # the core procedure: frequency list
    mfw.list.of.all = sort(table(c(wordlist.of.primary.set, wordlist.of.secondary.set)),
                            decreasing = TRUE)
    # if the whole list is long, then cut off the tail (e.g., > 5000 mfw)
      if (length(mfw.list.of.all) > mfw.list.cutoff) {
        mfw.list.of.all = mfw.list.of.all[1:mfw.list.cutoff]
      }
    # the only thing we need are words ordered by frequency (no frequencies)
    mfw.list.of.all = names(mfw.list.of.all)

    # Saving the list of features.
    # some comments into the file containing wordlist
    cat("# This file contains the words that were used for building the table",
      "# of frequencies. It can be also used for the next tasks, and for this",
      "# purpose it can be manually revised, edited, deleted, culled, etc.",
      "# You can either delete unwanted words, or mark them with \"#\"",
      "# -----------------------------------------------------------------------",
      "", file = "wordlist.txt", sep = "\n")
    # the current wordlist into a file
      # checking if encoding conversion is needed
      if(encoding == "native.enc") {
        data.to.be.saved = mfw.list.of.all
      } else {
        data.to.be.saved = iconv(mfw.list.of.all, to=encoding)
      }
  # writing the stuff
  cat(data.to.be.saved, file = "wordlist.txt", sep = "\n", append = TRUE)

  }   # <----- conditional expr. if(features.exist == TRUE) terminates here







  # empty the dump-dir if it already existed and create it if it did not previously exist
  if(dump.samples == TRUE){
	  if (file.exists("sample_dump_primary_set")){
		# a dump-dir seems to have been created during a previous run
		# tmp delete the dump-dir to remove all of its previous contents
		unlink("sample_dump_primary_set", recursive = TRUE)
	  }
	# (re)create the dump-dir
	dir.create("sample_dump_primary_set")
    # writing the stuff into files
    setwd("sample_dump_primary_set")
      for(i in names(corpus.of.primary.set)) {
        cat(corpus.of.primary.set[[i]], file = paste(names(corpus.of.primary.set[i]), ".txt", sep = ""))
      }
    setwd("..")
  }
  # empty the dump-dir if it already existed and create it if it did not previously exist
  if(dump.samples == TRUE){
	  if (file.exists("sample_dump_secondary_set")){
		# a dump-dir seems to have been created during a previous run
		# tmp delete the dump-dir to remove all of its previous contents
		unlink("sample_dump_secondary_set", recursive = TRUE)
	  }
	# (re)create the dump-dir
	dir.create("sample_dump_secondary_set")
    # writing the stuff into files
    setwd("sample_dump_secondary_set")
      for(i in names(corpus.of.secondary.set)) {
        cat(corpus.of.secondary.set[[i]], file = paste(names(corpus.of.secondary.set[i]), ".txt", sep = ""))
      }
    setwd("..")
  }







  # blank line on the screen
  message("")


  # preparing a huge table of all the frequencies for the training set
  freq.I.set.0.culling = make.table.of.frequencies(corpus = corpus.of.primary.set,
                            features = mfw.list.of.all,
                            absent.sensitive = FALSE,
                            relative = relative.frequencies)

  # preparing a huge table of all the frequencies for the test set
  freq.II.set.0.culling = make.table.of.frequencies(corpus = corpus.of.secondary.set,
                            features = mfw.list.of.all,
                            absent.sensitive = FALSE,
                            relative = relative.frequencies)

  # writing the frequency tables to text files (they can be re-used!)
  # first, the training set
      # checking if any re-encoding is needed
      if(encoding == "native.enc") {
        data.to.be.saved = t(freq.I.set.0.culling)
      } else {
        data.to.be.saved = t(freq.I.set.0.culling)
        rownames(data.to.be.saved) = iconv(rownames(data.to.be.saved), to=encoding)
        colnames(data.to.be.saved) = iconv(colnames(data.to.be.saved), to=encoding)
      }
  # writing the stuff
  write.table(data.to.be.saved, file = "freq_table_primary_set.txt")

  # now, the test set
      # checking if any re-encoding is needed
      if(encoding == "native.enc") {
        data.to.be.saved = t(freq.II.set.0.culling)
      } else {
        data.to.be.saved = t(freq.II.set.0.culling)
        rownames(data.to.be.saved) = iconv(rownames(data.to.be.saved), to=encoding)
        colnames(data.to.be.saved) = iconv(colnames(data.to.be.saved), to=encoding)
      }
  # writing the stuff
  write.table(data.to.be.saved, file = "freq_table_secondary_set.txt")


}
###############################################################################

# #################################################
# the module for loading the corpus terminates here
# #################################################



# #################################################
# module for exporting config settings
# #################################################

# Finally, we want to save some variable values for later use
cat("", file = "classify_config.txt", append = FALSE)
var.name <- function(x) {
      if(is.character(x) == TRUE) {
      cat(paste(deparse(substitute(x)), " = \"", x, "\"", sep = ""), file = "classify_config.txt", sep = "\n", append = TRUE)
        } else {
          cat(paste(deparse(substitute(x)), x, sep = " = "), file = "classify_config.txt", sep = "\n", append = TRUE) }
        }

 var.name(corpus.format)
 var.name(corpus.lang)
 var.name(analyzed.features)
 var.name(ngram.size)
 var.name(mfw.min)
 var.name(mfw.max)
 var.name(mfw.incr)
 var.name(start.at)
 var.name(culling.min)
 var.name(culling.max)
 var.name(culling.incr)
 var.name(mfw.list.cutoff)
 var.name(delete.pronouns)
 var.name(preserve.case)
 var.name(encoding)
 var.name(use.existing.freq.tables)
 var.name(use.existing.wordlist)
 var.name(classification.method)
 var.name(culling.of.all.samples)
 var.name(z.scores.of.all.samples)
 var.name(reference.wordlist.of.all.samples)
 var.name(distance.measure)
 var.name(svm.kernel)
 var.name(svm.degree)
 var.name(svm.coef0)
 var.name(svm.cost)
 var.name(k.value)
 var.name(l.value)
 var.name(sampling)
 var.name(sample.size)
 var.name(number.of.samples)
 var.name(final.ranking.of.candidates)
 var.name(how.many.correct.attributions)
 var.name(number.of.candidates)
 var.name(save.distance.tables)
 var.name(save.analyzed.features)
 var.name(save.analyzed.freqs)




# #################################################







# #################################################
# MAIN PROGRAM; the main loop is below
# #################################################


# cleaning the outputfile
cat("", file = outputfile, append = FALSE)

# saving the original mfw.max value in mfw.max.original
# this is useful for subtitles of bootstrap graphs
mfw.max.original = mfw.max

# the general counter for different purposes: initiation
number.of.current.iteration = 0

# useful for diagnostic reasons; this will be reported in the logfile
total.no.of.correct.attrib = c()
total.no.of.possible.attrib = c()

# retrieving the total number of texts to be "guessed"
# (anonymous texts and unique authorial samples will not be counted)
classes.train = c(gsub("_.*", "", rownames(freq.I.set.0.culling)))
classes.test = c(gsub("_.*", "", rownames(freq.II.set.0.culling)))
perfect.guessing = length(classes.test[classes.test %in% classes.train])







# #################################################
# module for culling (THE MAIN LOOP IN THE PROGRAM)
# #################################################
# #################################################
# #################################################
# module for culling (THE MAIN LOOP IN THE PROGRAM)
# #################################################


# testing if desired culling settings are acceptable;
# if too large, it is set to maximum possible
  if(culling.max > 100) {
  culling.max = 100
  }
  if(culling.min > 100) {
  culling.min = 100
  }
# if too small, it is set to 0 (i.e. minimal value)
  if(culling.min < 0) {
  culling.min = 0
  }
# if max value is LOWER than min value, make them equal
  if(culling.max < culling.min) {
  culling.max = culling.min
  }
# avoiding infinite loops
  if(culling.incr <= 1) {
  culling.incr = 10
  }

# #################################################



for(j in (culling.min/culling.incr):(culling.max/culling.incr)) {


        current.culling = j * culling.incr


        # applying culling
        # an additional table composed of relative word frequencies
        # of joint primary and secondary sets
        if(culling.of.all.samples == FALSE) {
                # applying the function culling to the I set
                primary.set = perform.culling(freq.I.set.0.culling,
                                             current.culling)
                # selecting the same variables from the II set
                secondary.set = freq.II.set.0.culling[,colnames(primary.set)]
        } else {
                # combining the two sets
                freq.table.both.sets = rbind(freq.I.set.0.culling,
                                             freq.II.set.0.culling)
                # applying the culling function to the combined table
                freq.table.both.sets = perform.culling(freq.table.both.sets,
                                             current.culling)
                # split the combined table into two sets again
                primary.set = freq.table.both.sets[rownames(freq.I.set.0.culling),]
                secondary.set = freq.table.both.sets[rownames(freq.II.set.0.culling),]
        }



        # additionally, deleting pronouns (if applicable)
        if(delete.pronouns == TRUE) {
                primary.set = delete.stop.words(primary.set, pronouns)
                secondary.set = delete.stop.words(secondary.set, pronouns)
        }


        # optionally, deleting stop words
        if(is.vector(stop.words) == TRUE) {
                primary.set = delete.stop.words(primary.set, stop.words)
                secondary.set = delete.stop.words(secondary.set, stop.words)
        }




# #################################################
# culling is done, but we are still inside the main loop





# starting the frequency list at frequency rank set in option start.at above

# TO SAY THE TRUTH, IT CAN BE DONE MUCH EARLIER: at the moment when
# the frequency list for either I set or both sets is produced,
# it can be cut and used for building freq. tables

primary.set = primary.set[,start.at:length(primary.set[1,])]
secondary.set = secondary.set[,start.at:length(secondary.set[1,])]




# Testing if the desired MFW number is acceptable,
# if MFW too large, it is set to maximum possible.
  if(mfw.max > length(primary.set[1,])) {
  mfw.max = length(primary.set[1,])
  }
# if too small, it is set to 2 (i.e., minimal value)
  if(mfw.min < 2) {
  mfw.min = 2
  }
# if the max value is smaller than the min value, it will be adjusted
  if(mfw.max < mfw.min) {
  mfw.max = mfw.min
  }
# avoiding infinite loops
  if( (mfw.max != mfw.min) && (mfw.incr == 0) ) {
  mfw.incr = 10
  }





message("")
message("culling @ ", current.culling, "\t", "available words ",
                  length(primary.set[1,]))







# an additional table composed of relative word frequencies
# of joint primary and secondary sets
freq.table.both.sets = rbind(primary.set, secondary.set)



# Entropy distance: experimental, but entirely available
# (the results do not really differ than for typical word frequencies)
#
# A = t(t(freq.table.both.sets + 1) / colSums(freq.table.both.sets + 1))
# B = t(t(log(freq.table.both.sets + 2)) / -(colSums(A * log(A))))
# freq.table.both.sets = B
#


if(tolower(classification.method) == "delta") {
        # a short message on the screen:
        if(distance.measure == "delta") {
                message("Calculating classic Delta distances...")
        }
        if(distance.measure == "argamon") {
                message("Calculating Argamon's Delta distances...")
        }
        if(distance.measure == "eder") {
                message("Calculating Eder's Delta distances...")
        }
        if(distance.measure == "eder") {
                message("Calculating Eder's Simple distances...")
        }
        if(distance.measure == "manhattan") {
                message("Calculating Manhattan distances...")
        }
        if(distance.measure == "canberra") {
                message("Calculating Canberra distances...")
        }
        if(distance.measure == "euclidean") {
                message("Calculating Euclidean distances...")
        }
        if(distance.measure == "cosine") {
                message("Calculating Cosine distances...")
        }
}










# #################################################
# the internal loop starts here (for i = mfw.min : mfw.max)
# #################################################



for(i in seq(mfw.min, mfw.max, round(mfw.incr)) ) {

    
mfw = i



# for safety reasons, if MFWs > words in samples
if(mfw > length(colnames(freq.table.both.sets)) ) {
  mfw = length(colnames(freq.table.both.sets))
  }



# the current task (number of MFW currently analyzed) echoed on the screen
message(mfw, " ", appendLF = FALSE)































if(tolower(classification.method) == "delta") {
  classification.results = perform.delta(training.set = primary.set[,1:mfw],
                                test.set = secondary.set[,1:mfw],
                                distance = distance.measure,
                                z.scores.both.sets = z.scores.of.all.samples)
  distance.table = attr(classification.results, "distance.table")

}


if(tolower(classification.method) == "knn") {
  classification.results = perform.knn(training.set = primary.set[,1:mfw],
                                       test.set = secondary.set[,1:mfw],
                                       k.value = k.value)
}


if(tolower(classification.method) == "svm") {
  classification.results = perform.svm(training.set = primary.set[,1:mfw],
                                       test.set = secondary.set[,1:mfw])
}


if(tolower(classification.method) == "naivebayes") {
  classification.results = perform.naivebayes(training.set= primary.set[,1:mfw],
                                              test.set = secondary.set[,1:mfw])
}


if(tolower(classification.method) == "nsc") {
  classification.results = perform.nsc(training.set = primary.set[,1:mfw],
                                       test.set = secondary.set[,1:mfw],
                                       show.features = show.features)
}





expected_classes = gsub("_.*","",rownames(secondary.set))
predicted_classes = as.vector(classification.results)

#performance = performance.measures(expected_classes, predicted_classes)







# returns the ranking of the most likely candidates as a list
if(final.ranking.of.candidates == TRUE) {
    cat("\n\n\n", file = outputfile, append = TRUE)
      misclassified.samples =
                   paste(rownames(secondary.set), "\t-->\t",
                   classification.results)[classes.test != classification.results]
      cat(misclassified.samples, file = outputfile, append = TRUE, sep = "\n")
      # temporarily (the results should be made available, eventually)
      rm(misclassified.samples)
}




# returns the number of correct attributions
if(how.many.correct.attributions == TRUE) {
    no.of.correct.attrib = sum(as.numeric(classes.test ==
                                 classification.results))
    total.no.of.correct.attrib =
         c(total.no.of.correct.attrib, no.of.correct.attrib)
    total.no.of.possible.attrib =
         c(total.no.of.possible.attrib, perfect.guessing)
    cat("\n", file = outputfile, append = TRUE)
    cat(mfw, " MFW , culled @ ", current.culling, "%,  ",
         no.of.correct.attrib, " of ", perfect.guessing, "\t(",
         round(no.of.correct.attrib / perfect.guessing * 100, 1), "%)",
         "\n", file = outputfile, append = TRUE, sep = "")
}











if(cv.folds > 0) {

  message("")
  message("cross-validation...")


  #bootstrap.output = "bootstrap_output.txt"
  #cleaning the bootstrapfile
  #cat("", file = bootstrap.output, append = FALSE)

  # creating an empty matrix for the final success scores
  cross.validation.results = c()
  cross.validation.results.all = c()
  
  
  # accumulating the predictions and the expected classes
  predicted_classes = c()
  expected_classes = c()




  # beginning of k-fold cross-validation (k being the number of iterations)
  for(iterations in 1 : cv.folds) {

    # an additional table combined of frequencies of set I and II
    # just for feeding the bootstrap module
    freq.table.both.sets.binded = rbind(primary.set[,1:mfw], secondary.set[,1:mfw])


    names.of.training.set.orig = rownames(primary.set)
    classes.training.set = gsub("_.*", "", rownames(primary.set))
    classes.test.set = gsub("_.*", "", rownames(secondary.set))
    names.both.sets = rownames(freq.table.both.sets.binded)
    classes.both.sets = c(classes.training.set, classes.test.set)

    training.samples = c()
    test.samples = c()


      # this looks for classes that were not represented so far in I set
      for(i in names(table(classes.training.set)) ) {
        #
        # count the number of samples of class i included originally in I set
        no.of.training.samples = sum(as.numeric(classes.training.set == i))
        # determine the class' name, surround the name with word boundary char
        class.name = paste("\\b", i, "\\b", sep = "")
        # in both sets, identify the positions of current class' samples
        pinpoint.samples = grep(class.name, classes.both.sets)
        # sanity check, just in case
        if(length(pinpoint.samples) > no.of.training.samples) {
                # select randomly N items from the pinpoited positions
                training = sample(pinpoint.samples, no.of.training.samples)
                # identify the remaining ones: future test set samples
                test = setdiff(pinpoint.samples, training)
                # pick the names at the positions identified above
                training.samples = c(training.samples, names.both.sets[training])
                # the remaining ones go to the test set
                test.samples = c(test.samples, names.both.sets[test])
        } else {
                test = pinpoint.samples
                test.samples = c(test.samples, names.both.sets[test])
        }
      }


#### !!! Anon samples are excluded!!!


  # establishing the training set:
  training.set = freq.table.both.sets.binded[training.samples,]

  # establishing the test set
  test.set = freq.table.both.sets.binded[test.samples,]


 # zscores.training.set = zscores.table.both.sets[training.samples,]
 # zscores.test.set = zscores.table.both.sets[test.samples,]


  if(tolower(classification.method) == "delta") {
    classification.results = perform.delta(training.set,
                                   test.set,
                                   distance = distance.measure,
                                   z.scores.both.sets = z.scores.of.all.samples)
    distance.table = attr(classification.results, "distance.table")
  }
  if(tolower(classification.method) == "knn") {
    classification.results = perform.knn(training.set, test.set, k.value)
  }
  if(tolower(classification.method) == "svm") {
    classification.results = perform.svm(training.set, test.set)
  }
  if(tolower(classification.method) == "nsc") {
    classification.results = perform.nsc(training.set, test.set,
                                         show.features = show.features)
  }
  if(tolower(classification.method) == "naivebayes") {
    classification.results = perform.naivebayes(training.set, test.set)
  }



  # retrieving classes of the new training set
  classes.training = gsub("_.*", "", rownames(training.set))

  # retrieving classes of the new test set
  classes.test = gsub("_.*", "", rownames(test.set))

  
  
  # accumulating the predictions and the expected classes
  predicted_classes = c(predicted_classes, as.vector(classification.results))
  expected_classes = c(expected_classes, classes.test)


    # returns the number of correct attributions
    if(how.many.correct.attributions == TRUE) {
          no.of.correct.attrib = sum(as.numeric(classes.test ==
                                     classification.results))
      # getting the max. number of samples that couold be guessed
      perfect.guessing.cv = sum(as.numeric(classes.test %in% classes.training))
      cat("\n", file = outputfile, append = TRUE)
      cat(mfw, " MFW , culled @ ", current.culling, "%,  ",
               no.of.correct.attrib, " of ", perfect.guessing.cv, "\t(",
               round(no.of.correct.attrib / perfect.guessing.cv * 100, 1), "%)",
               "\n", file = outputfile, append = TRUE, sep = "")
      # percentage of correct attributions
      success.rate.cv = no.of.correct.attrib / perfect.guessing.cv * 100
      # combining results for k folds
      cross.validation.results = c(cross.validation.results, success.rate.cv)
    }

  }


  cross.validation.results.all = cbind(cross.validation.results.all, cross.validation.results)
  colnames(cross.validation.results.all) = paste(mfw, "@", current.culling, sep="")
  
#  performance = performance.measures(expected_classes, predicted_classes)

  
}   # <-- if(cv.folds > 0)






if(exists("cross.validation.results.all")) {
  cross.validation.summary = cbind(cross.validation.summary, cross.validation.results.all)
  rownames(cross.validation.summary) = 1:cv.folds
}





# saving a requested stuff into external files

# writing distance table(s) to a file (if an appropriate option has been chosen)
if(save.distance.tables == TRUE && exists("distance.table") == TRUE) {
  distance.table.filename = paste("distance_table_", mfw, "mfw_", current.culling, "c.txt", sep = "")
    # checking if encoding conversion is needed
    if(encoding == "native.enc") {
      data.to.be.saved = distance.table
    } else {
      data.to.be.saved = distance.table
      rownames(data.to.be.saved) = iconv(rownames(data.to.be.saved), to = encoding)
      colnames(data.to.be.saved) = iconv(colnames(data.to.be.saved), to = encoding)
    }
  # writing the stuff
  write.table(file = distance.table.filename, data.to.be.saved)
}

# writing the words (or features) actually used in the analysis
features.actually.used = colnames(freq.table.both.sets[,1:mfw])
#
if(save.analyzed.features == TRUE) {
    # checking if encoding conversion is needed
    if(encoding == "native.enc") {
      data.to.be.saved = features.actually.used
    } else {
      data.to.be.saved = iconv(features.actually.used, to = encoding)
    }
  # writing the stuff
  cat(data.to.be.saved,
     file = paste("features_analyzed_", mfw, "mfw_", current.culling, "c.txt", sep = ""),
     sep = "\n")
}

# writing the frequency table that was actually used in the analysis
if(save.analyzed.freqs == TRUE) {
    # checking if encoding conversion is needed
    if(encoding == "native.enc") {
      data.to.be.saved = t(freq.table.both.sets[,1:mfw])
    } else {
      data.to.be.saved = t(freq.table.both.sets[,1:mfw])
      rownames(data.to.be.saved) = iconv(rownames(data.to.be.saved), to = encoding)
      colnames(data.to.be.saved) = iconv(colnames(data.to.be.saved), to = encoding)
    }
  # writting the stuff -- the file name will be changed accordingly
  write.table(data.to.be.saved,
     file = paste("frequencies_analyzed_", mfw, "mfw_", current.culling, "c.txt", sep = ""))
}









#######
####### the features, confusion matrices, etc., should be captured here!!!!!!
#######






}    # <-- the internal loop for(i) returns here
# #################################################

# blank line on the screen
message("")


}    # <-- the main loop for(j) returns here
# #################################################



all.guesses = total.no.of.correct.attrib / total.no.of.possible.attrib * 100
total.no.of.correct.attrib = sum(total.no.of.correct.attrib)
total.no.of.possible.attrib = sum(total.no.of.possible.attrib)



# information about the current task into the logfile
cat("\nGeneral attributive success:  ", total.no.of.correct.attrib, " of ",
           total.no.of.possible.attrib, " (",
           round(total.no.of.correct.attrib/total.no.of.possible.attrib*100, 1),
           "%)\n", file = outputfile, append = TRUE, sep = "")
cat("\nMFWs from ", mfw.min, " to ", mfw.max.original,
                  " @ increment ", mfw.incr, "\nCulling from ", culling.min,
                  " to ", culling.max, " @ increment ", culling.incr,
                  "\nPronouns deleted: ", delete.pronouns, "\n",
				  file = outputfile, append = TRUE, sep = "")
# additional empty line in outputfile (EOF)
cat("\n", file = outputfile, append = TRUE)


# the same information (about the current task) on screen
message("\nGeneral attributive success:  ", total.no.of.correct.attrib, " of ",
           total.no.of.possible.attrib, " (",
           round(total.no.of.correct.attrib/total.no.of.possible.attrib*100,1),
           "%, sd =", round(sd(all.guesses),1),"%)")
message("\nMFWs from ", mfw.min, " to ", mfw.max.original,
                  " @ increment ", mfw.incr, "\nCulling from ", culling.min,
                  " to ", culling.max, " @ increment ", culling.incr,
                  "\nPronouns deleted: ", delete.pronouns, sep = "")
message("")


###########################################################




# Names of many variables are incredibly unfashionable: they were acceptable
# in ver. 0.0.1 of the script, which provided just a basic Delta test
# with no additional options. Since it is quite a lot of work to modernize
# the variables' names in the code (and parhaps it is too late now...),
# these simple wrappers will rename at least the variables to be exported:
success.rate = all.guesses
  if(length(success.rate) >1) {
    overall.success.rate = mean(all.guesses)
  }
frequencies.training.set = freq.I.set.0.culling
frequencies.test.set = freq.II.set.0.culling
frequencies.both.sets = freq.table.both.sets
features.actually.used = colnames(freq.table.both.sets[,1:mfw])
features = mfw.list.of.all


distinctive.features = attr(classification.results, "features")


# what about removing some of the variables? (suppose there are thousands
# of texts and dozens of features, and only 2GB of RAM...)







# #################################################
# praparing final resutls: building a class




if(exists("misclassified.samples")) {
  attr(misclassified.samples, "description") = "texts (samples) that were not correctly classified"
}
if(exists("cross.validation.summary") & length(cross.validation.summary) >0 ) {
  attr(cross.validation.summary, "description") = "correctly guessed samples (cross-validation folds)"
  if(dim(as.matrix(cross.validation.summary))[2] >1) {
    class(cross.validation.summary) = c("stylo.data", "matrix")
  }
}
if(exists("success.rate")) {
  attr(success.rate, "description") = "percentage of correctly guessed samples"
  class(success.rate) = "stylo.data"
}
if(exists("overall.success.rate")) {
  attr(overall.success.rate, "description") = "average percentage of correctly guessed samples"
  class(overall.success.rate) = "stylo.data"
}
if(exists("distance.table")) {
  attr(distance.table, "description") = "final distances between each pair of samples"
  class(distance.table) = "stylo.data"
}
if(exists("distinctive.features") & length(distinctive.features) > 0) {
  attr(distinctive.features, "description") = "most distinctive features"
}
if(exists("frequencies.both.sets")) {
  attr(frequencies.both.sets, "description") = "frequencies of words/features accross the corpus"
  class(frequencies.both.sets) = "stylo.data"
}
if(exists("features") & length(features) >0 ) {
  attr(features, "description") = "features (e.g. words, n-grams, ...) applied to data"
  class(features) = "stylo.data"
}
if(exists("features.actually.used")) {
  attr(features.actually.used, "description") = "features (e.g. frequent words) actually analyzed"
  class(features.actually.used) = "stylo.data"
}
if(exists("zscores.both.sets")) {
  attr(zscores.both.sets, "description") = "z-scored frequencies accross the whole corpus"
  class(zscores.both.sets) = "stylo.data"
}
if(exists("frequencies.training.set")) {
  attr(frequencies.training.set, "description") = "frequencies of words/features in the training set"
  class(frequencies.training.set) = "stylo.data"
}
if(exists("frequencies.test.set")) {
  attr(frequencies.test.set, "description") = "frequencies of words/features in the test set"
  class(frequencies.test.set) = "stylo.data"
}
if(exists("performance")) {
  attr(performance, "description") = "precision, recall, accuracy, and the f1 measure"
}
if(exists("predicted_classes")) {
  predicted = predicted_classes
  attr(predicted, "description") = "a vector of classes predicted by the classifier"
}
if(exists("expected_classes")) {
  expected = expected_classes
  attr(expected, "description") = "ground truth, or a vector of expected classes"
}






# creating an object (list) that will contain the final results,
# tables of frequencies, etc.etc.
results.classify = list()
# elements that we want to add on this list
variables.to.save = c("misclassified.samples",
                      "success.rate",
                      "overall.success.rate",
                      "performance",
                      "predicted",
                      "expected",
                      "distance.table",
                      "distinctive.features",
                      "features",
                      "features.actually.used",
                      "zscores.both.sets",
                      "frequencies.both.sets",
                      "frequencies.training.set",
                      "cross.validation.summary",
                      "frequencies.test.set")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.classify[[i]] = get(i)
}



# adding some information about the current function call
# to the final list of results
results.classify$call = match.call()
results.classify$name = call("classify")


# This assings the list of final resutls to the class "stylo.resutls";
# the same class will be used to handle the output of stylo(),
# rolling.delta() and oppose(). See the files "print.stylo.results.R"
# and "summary.stylo.results.R" (no help files are provided, since
# these two functions are not visible for the users).
class(results.classify) = "stylo.results"






# back to the original working directory
setwd(original.path)

# return the value of the function
return(results.classify)
}
