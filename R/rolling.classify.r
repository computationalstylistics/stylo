

rolling.classify = function(gui = FALSE,
         training.corpus.dir = "reference_set",
         test.corpus.dir = "test_set",
         training.frequencies = NULL,
         test.frequencies = NULL,
         training.corpus = NULL,
         test.corpus = NULL,
         features = NULL,
         path = NULL,
         slice.size = 5000,
         slice.overlap = 4500,
         training.set.sampling = "no.sampling",
         mfw = 100,
         culling = 0,
         milestone.points = NULL,
         milestone.labels = NULL,
         plot.legend = TRUE,
         add.ticks = FALSE, ...) {



# if any command-line arguments have been passed by a user, they will
# be stored on the following list and used to overwrite the defaults
passed.arguments = list(...)






# saving custom height and width of the final picture (if specified by the user)
save.plot.custom.width = passed.arguments$plot.custom.width
save.plot.custom.height = passed.arguments$plot.custom.height



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
  cat("using current directory...\n")
}

# Choose directory via GUI:
#
# Just a few lines that allow users to choose the working directory if working
# with the GUI.

if(gui == TRUE & is.null(path)){
  selected.path = tk_choose.dir(caption = "Select your working directory. It should a subdirectory called *corpus* ")
  setwd(selected.path)
}



if(is.character(training.corpus.dir)==FALSE | nchar(training.corpus.dir)==0) {
  training.corpus.dir = "reference_set"
}
if(is.character(test.corpus.dir) == FALSE | nchar(test.corpus.dir) == 0) {
  test.sample.dir = "test_set"
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
        #variables = gui.classify(...)
        cat("\n")
        cat("GUI could not be launched -- it is not supported yet :-( \n\n")
      } else {
        cat("\n")
        cat("GUI could not be launched -- default settings will be used;\n")
        cat("otherwise please pass your variables as command-line agruments\n\n")
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
custom.graph.title = variables$custom.graph.title
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
#plot.custom.height = variables$plot.custom.height
#plot.custom.width = variables$plot.custom.width
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
custom.graph.filename = variables$custom.graph.filename
show.features = variables$show.features

# #############################################################################
# additional options for dealing with custom picture size

# additional settings relavant to rolling.classify only
# (overriding the custom settings for the size of final graphs)
plot.custom.width = 10
plot.custom.height = 4

# these are overwritten, if the user specified some values
if(length(save.plot.custom.width) > 0) {
      # if the saved value exists, apply it to the main variable
      plot.custom.width = save.plot.custom.width
}
# these are overwritten, if the user specified some values
if(length(save.plot.custom.height) > 0) {
      # if the saved value exists, apply it to the main variable
      plot.custom.height = save.plot.custom.height
}






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








# #################################################
# the module for loading a corpus from text files;
# it can be omitted if the frequency table for
# both training and test sets already exist
# (then "use.existing.freq.tables" should be set to TRUE
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
      } else {
        cat("\n")
        cat("You seem to have chosen an existing set of features\n")
        cat("Unfortunately, something is wrong: check if your variable\n")
        cat("has a form of vector\n")
        cat("\n")
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
        cat("\n", "reading a custom set of features from a file...", "\n",sep="")
        # reading a file: newlines are supposed to be delimiters
        features = scan(features,what="char",sep="\n",encoding=encoding)
        # getting rid of the lines beginning with the "#" char
        features = c(grep("^[^#]",features,value=TRUE))
      } else {
        # if there's no such a file, then don't try to use it
        cat("\n", "file \"",features, "\" could not be found\n",sep="")
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
        cat("\n")
        cat("You seem to have chosen an existing table with frequencies\n")
        cat("Unfortunately, something is wrong: check if your variable\n")
        cat("has a form of matrix/data frame\n")
        cat("\n")
        stop("Wrong format of the table of frequencies")
      }
      # this code makes sure that the table has variables' names
      if(length(colnames(frequencies)) == 0) {
        colnames(frequencies) = paste("var",1:length(frequencies[1,]),sep="_")
      }
      # this code makes sure that the table has samples' names
      if(length(rownames(frequencies)) == 0) {
        rownames(frequencies) = paste("sample",1:length(frequencies[,1]),sep="_")
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
        cat("\n", "reading a file containing frequencies...", "\n",sep="")
        frequencies = t(read.table(frequencies, encoding=encoding))
      } else {
        # if there's no such a file, then don't try to use it
        cat("\n", "file \"",frequencies, "\" could not be found\n",sep="")
        stop("Wrong file name")
      }
    # selecting the above matrix as a valid corpus
    corpus.exists = TRUE
  }



  # If a custom set of features was indicated, try to pick the matching variables only
  if(features.exist == TRUE & corpus.exists == TRUE) {
      # checking if the chosen features do match the columns of the table
      if(length(grep("TRUE",colnames(frequencies) %in% features)) < 2) {
        cat("The features you want to analyze do not match the variables' names:\n")
        cat("\n")
        cat("Available features:",head(colnames(frequencies)), "...\n")
        cat("Chosen features:", head(features), "...\n")
        cat("\n")
        cat("Check the rotation of your table and the names of its rows and columns.\n")
        stop("Input data mismatch")
      } else {
        # if everything is right, select the subset of columns from the table:
        frequencies = frequencies[,colnames(frequencies) %in% features]
      }
  }


  # If no custom features were chosen, take them from the variables' names
  if(features.exist == FALSE & corpus.exists == TRUE) {
     features = colnames(frequencies)
  }



  # Additionally, check if the table with frequencies is long enough
  if(corpus.exists == TRUE) {
    if(length(frequencies[,1]) < 2 | length(frequencies[1,]) < 2 ) {
      cat("\n")
      cat("There is not enough samples and/or features to be analyzed.\n")
      cat("Try to use tables of at least two rows by two columns.\n")
      cat("\n")
      stop("Wrong size of the table of frequencies")
    }
  }

  # 1st iteration: setting the matrix containing the training set (if applicable)
  if(corpus.exists == TRUE & iteration == 1) {
    freq.I.set.0.culling = frequencies
    cat("Training set successfully loaded.\n")
  }
  # 2nd iteration: setting the matrix containing the test set (if applicable)
  if(corpus.exists == TRUE & iteration == 2) {
    freq.II.set.0.culling = frequencies
    cat("Test set successfully loaded.\n")
  }

# attempts at loading the training set and the test set: the loop returns here
}

# Two iterations completed, another sanity check should be applied
  # First, let's check if the I set was loaded
  if(!exists("freq.I.set.0.culling") & exists("freq.II.set.0.culling")) {
    cat("Training set is missing, though.\n")
    cat("Trying to build both tables from scratch.\n")
    corpus.exists = FALSE
  }
  # Secondly, let's check the II set
  if(exists("freq.I.set.0.culling") & !exists("freq.II.set.0.culling")) {
    cat("Test set is missing, though.\n")
    cat("Trying to build both tables from scratch.\n")
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
            names(parsed.corpus) = paste("sample",1:length(parsed.corpus),sep="_")
          }
        # if everything is fine, use this variable as a valid corpus
#        loaded.corpus = parsed.corpus
      } else {
        cat("\n")
        cat("The object you've specified as your corpus cannot be used.\n")
        cat("It should be a list containing particular text samples\n")
        cat("(vectors containing sequencies of words/n-grams or other features).\n")
        cat("The samples (elements of the list) should have their names.\n")
        cat("Alternatively, try to build your corpus from text files (default).\n")
        cat("\n")
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
      cat("Two subcorpora loaded successfully.\n")
      corpus.exists = TRUE
    } else {
      cat("The subcorpora will be loaded from text files...\n")
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

  # Retrieving the names of samples
  #
  filenames.primary.set = list.files(training.corpus.dir)
  filenames.secondary.set = list.files(test.corpus.dir)

  # Checking whether required files and subdirectories exist
  if(file.exists(training.corpus.dir) == FALSE | file.exists(test.corpus.dir) == FALSE) {
    cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "Working directory should contain two subdirectories:
        \"",training.corpus.dir,"\" and \"",test.corpus.dir,"\"\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }
  # Checking if the subdirectories contain any stuff
  if(length(filenames.primary.set) <2 | length(filenames.secondary.set) >1) {
    cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "The subdirectory \"",training.corpus.dir,"\" should contain at least",
        " two text samples; \n\"",
        test.corpus.dir,"\" should contain exactly one sample!\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }


# loading the sample to be rolled through, looking for milestone marks;
# don't mind stupid names of variables (e.g. 'corpus.of.secondary.set')
corpus.of.secondary.set = load.corpus.and.parse(files=filenames.secondary.set,
                           corpus.dir = test.corpus.dir,
                           encoding = encoding,
                           markup.type = corpus.format,
                           corpus.lang = corpus.lang,
                           splitting.rule = splitting.rule,
                           preserve.case = preserve.case,
                           sampling = "no.sampling"
                           )

milestone.points = grep("xmilestone", corpus.of.secondary.set[[1]])
text.length = length(corpus.of.secondary.set[[1]])


# once more: loading the sample to be rolled through
corpus.of.secondary.set = load.corpus.and.parse(files = filenames.secondary.set,
                           corpus.dir = test.corpus.dir,
                           encoding = encoding,
                           markup.type = corpus.format,
                           corpus.lang = corpus.lang,
                           splitting.rule = splitting.rule,
                           preserve.case = preserve.case,
                           sample.size = slice.size,
                           sampling = "normal.sampling",  # moving window!
                           sampling.with.replacement = sampling.with.replacement,
                           sample.overlap = slice.overlap, # moving window!!!
                           number.of.samples = number.of.samples,
                           features = analyzed.features,
                           ngram.size = ngram.size)


# loading text files, splitting, parsing, n-gramming, samping, and so forth
corpus.of.primary.set = load.corpus.and.parse(files = filenames.primary.set,
                           corpus.dir = training.corpus.dir,
                           encoding = encoding,
                           markup.type = corpus.format,
                           corpus.lang = corpus.lang,
                           splitting.rule = splitting.rule,
                           preserve.case = preserve.case,
                           sample.size = slice.size,
                           sampling = training.set.sampling,
                           sampling.with.replacement = sampling.with.replacement,
                           sample.overlap = 0, # no moving window, this time!
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
  cat("\n")

  # both corpora (training set and test set) shoud contain some texts;
  # if the number of text samples is lower than 2, the script will stop
  # (in the case of the test set, it MUST contain exacly one sample!)
  if(length(corpus.of.primary.set) < 2 || length(corpus.of.secondary.set) < 1) {
    cat("\n\n","either the training set or the test set is empty!", "\n\n")
    stop("corpus error")
  }

  # If an external vector of features (usually: the most frequent words) has not
  # been specified (cf. the argument "features"), then we need a list of the most
  # frequent words (or n-grams, or anything else) used in the current corpus,
  # in descending order, without frequencies (just a list of words/features).
  if (features.exist == TRUE) {
    cat("\n")
    cat("using an existing wordlist (vector of features)...\n")
    # just to say something
    features = features
  } else {
    # Extracting all the words (features) used in the texts of primary set
    # (or both if "Z-scores all" is set to TRUE)
    wordlist.of.primary.set = c()
    cat("\n")
    # iterating over the samples stored in corpus.of.primary.set
    for (file in 1 : length(corpus.of.primary.set)) {
      # loading the next sample from the list filenames.primary.set,
      current.text = corpus.of.primary.set[[file]]
      # putting the samples together:
      wordlist.of.primary.set = c(wordlist.of.primary.set, current.text)
      # short message on screen
      cat(".")
      if(file/25 == floor(file/25)) { cat("\n")} # a newline every 25th sample
    }
    # including words of the secondary set in the reference wordlist (if specified)
      if (reference.wordlist.of.all.samples == TRUE) {
        wordlist.of.secondary.set = c()
        cat("\n")
        for (file in 1 : length(corpus.of.secondary.set)) {
          # loading the next sample from the list filenames.secondary.set,
          current.text = corpus.of.secondary.set[[file]]
          # putting samples together:
          wordlist.of.secondary.set = c(wordlist.of.secondary.set, current.text)
          # short message on screen
          cat(".")
          if(file/25 == floor(file/25)) { cat("\n")} # a newline every 25th sample
        }
      } else {
        # otherwise, create an empty vector
        wordlist.of.secondary.set = c()
      }

    # Preparing a sorted frequency list of the whole primary set (or both sets).
    # short message
    cat("\n")
    cat(length(c(wordlist.of.primary.set,wordlist.of.secondary.set)),"tokens",
         "will be used to create a list of features\n")
    # the core procedure: frequency list
    features = sort(table(c(wordlist.of.primary.set,wordlist.of.secondary.set)),
                            decreasing=T)
    # if the whole list is long, then cut off the tail (e.g., > 5000 mfw)
      if (length(features) > mfw.list.cutoff) {
        features = features[1:mfw.list.cutoff]
      }
    # the only thing we need are words ordered by frequency (no frequencies)
    features = names(features)

    # Saving the list of features.
    # some comments into the file containing wordlist
    cat("# This file contains the words that were used for building the table",
      "# of frequencies. It can be also used for the next tasks, and for this",
      "# purpose it can be manually revised, edited, deleted, culled, etc.",
      "# You can either delete unwanted words, or mark them with \"#\"",
      "# -----------------------------------------------------------------------",
      "", file="wordlist.txt", sep="\n")
    # the current wordlist into a file
      # checking if encoding conversion is needed
      if(encoding == "native.enc") {
        data.to.be.saved = features
      } else {
        data.to.be.saved = iconv(features, to=encoding)
      }
  # writing the stuff
  cat(data.to.be.saved,file="wordlist.txt", sep="\n",append=T)


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
        cat(corpus.of.primary.set[[i]],file=paste(names(corpus.of.primary.set[i]),".txt",sep=""))
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
        cat(corpus.of.secondary.set[[i]],file=paste(names(corpus.of.secondary.set[i]),".txt",sep=""))
      }
    setwd("..")
  }







  # blank line on the screen
  cat("\n")


  # preparing a huge table of all the frequencies for the training set
  freq.I.set.0.culling = make.table.of.frequencies(corpus = corpus.of.primary.set,
                            features = features,
                            absent.sensitive = FALSE,
                            relative = relative.frequencies)

  # preparing a huge table of all the frequencies for the test set
  freq.II.set.0.culling = make.table.of.frequencies(corpus = corpus.of.secondary.set,
                            features = features,
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
  write.table(data.to.be.saved, file = "freq_table_reference_set.txt")

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
  write.table(data.to.be.saved, file = "freq_table_sliced_sample.txt")


}
###############################################################################

# #################################################
# the module for loading the corpus terminates here
# #################################################




# #################################################
# culling, pronoun deletion, stop word list deletion


        # applying culling
        # an additional table composed of relative word frequencies
        # of joint primary and secondary sets
        if(culling.of.all.samples == FALSE) {
                # applying the function culling to the I set
                primary.set = perform.culling(freq.I.set.0.culling, culling)
                # selecting the same variables from the II set
                secondary.set = freq.II.set.0.culling[,colnames(primary.set)]
        } else {
                # combining the two sets
                freq.table.both.sets = rbind(freq.I.set.0.culling,
                                             freq.II.set.0.culling)
                # applying the culling function to the combined table
                freq.table.both.sets = perform.culling(freq.table.both.sets, culling)
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




training.set = primary.set[,start.at:mfw]
test.set = secondary.set[,start.at:mfw]










# #################################################
# the main part of the function begins!
# #################################################


# classification

if(tolower(classification.method) == "delta") {
  classification.results = perform.delta(training.set, test.set,
                                distance = distance.measure,
                                z.scores.both.sets = z.scores.of.all.samples)
  ylabel = "Delta classification"
}

if(tolower(classification.method) == "svm") {
  classification.results = perform.svm(training.set, test.set)
  ylabel = "SVM classification"
}

if(tolower(classification.method) == "nsc") {
  classification.results = perform.nsc(training.set, test.set,
                                       show.features = show.features)
  ylabel = "NSC classification"
}

if(tolower(classification.method) == "knn") {
  classification.results = perform.knn(training.set, test.set, k.value)
}

if(tolower(classification.method) == "naivebayes") {
  classification.results = perform.naivebayes(training.set,  test.set)
}





# Positioning in plots: replacing dummy sample names with their positions;
# the general rule is as follows:
# 1th: (slice.size / 2)
# 2nd: (slice.size / 2) + (slice.size - slice.overlap)
# 3rd: (slice.size / 2) + (slice.size - slice.overlap) + (slice.size - slice.overlap)
# Nth: (slice.size / 2) + ((slice.size - slice.overlap) * (N - 1))
names(classification.results) =  c(round(slice.size/2) +
                                    ((slice.size - slice.overlap) *
                                    (1:length(names(classification.results)) -1)) )
#


# #################################################
# ...and that's it: the main procedure is completed
# #################################################







# plotting


# it should be classes rather than mere rownames!!!!!!!!!!!!!!!!!!!!
colors.first.choice = assign.plot.colors((unique(gsub("_.+","",rownames(training.set)))), opacity=0.99, col = colors.on.graphs)
colors.second.choice = assign.plot.colors((unique(gsub("_.+","",rownames(training.set)))), opacity=0.6, col = colors.on.graphs)
colors.third.choice = assign.plot.colors((unique(gsub("_.+","",rownames(training.set)))), opacity=0.3, col = colors.on.graphs)





first.choice = attr(classification.results, "rankings")[,1]
names(first.choice) = names(classification.results)
second.choice = attr(classification.results, "rankings")[,2]
names(second.choice) = names(classification.results)
if(length(attr(classification.results, "rankings")[1,]) > 2) {
      third.choice = attr(classification.results, "rankings")[,3]
      names(third.choice) = names(classification.results)
}





# how many words/features the main sample has
entire.sample.length = (slice.size - slice.overlap) * length(classification.results)
  # if slice overlap was used, an additional tweak is needed
  if(slice.overlap >0) {
       entire.sample.length = entire.sample.length + slice.overlap
  }


# size of the assessed dataset (in tokens) + 10%
sample.length.with.margin = entire.sample.length + entire.sample.length * 0.1









plot.current.task = function(){
        # starting an empty plot
        plot(NULL, xlim = c(0, sample.length.with.margin), ylim=c(0,1), type="n",
             axes=FALSE, ylab=ylabel, xlab="story development (in words)")
        # adding an axis at the bottom
        axis(1, las=0)

        # adding vertical lines for each "xmilestone" string included in tested sample
        if(length(milestone.points) > 0){
                if(length(milestone.labels) == 0) {
                    identifiers = rep(unlist(strsplit("abcdefghijklmnopqrstuvwxyz","")),5)
                    identifiers = identifiers[1:length(milestone.points)]
                } else {
                    identifiers = milestone.labels
                }
                if(classification.method == "delta" & length(attr(classification.results, "rankings")[1,]) > 2) {
                    segments(milestone.points,0.62,milestone.points,0.8, lty=3)
                } else {
                    segments(milestone.points,0.47,milestone.points,0.8, lty=3)
                }
                segments(milestone.points,-0.1,milestone.points,0.1, lty=3)
                text(milestone.points, 0.85, labels=identifiers, cex=0.7, srt=90, adj=c(0,1))
        }

        # position of the gray rectangle that shows the slice size
        x = entire.sample.length-10*slice.size

        # drawing a rectangle; its length depending on the number of stripes
        if(classification.method == "delta" & length(attr(classification.results, "rankings")[1,]) > 2) {
            rect(x, c(-0.1,0.62), x+slice.size, c(0.13,1), border=NA, col=rgb(0.4,0.4,0.4,0.3))
        } else {
            rect(x, c(-0.1,0.47), x+slice.size, c(0.13,1), border=NA, col=rgb(0.4,0.4,0.4,0.3))
        }

        # adding arrows to show the slice size
        arrows(x, 0.95, x+slice.size, 0.95, length=0.05, code=3)
        # depending on relative position of the rectangle, adding a label either
        # on its right, or on the left side
        if(x < entire.sample.length/2) {
            text(x+slice.size, 0.95, paste(" ", slice.size, "words  "), cex=0.75, adj=c(0,0.5))
        } else {
            text(x, 0.95, paste(" ", slice.size, "words  "), cex=0.75, adj=c(1,0.5))
        }


        # using a very nice function rle() for identifying sequences of equal values
        end.segment = as.numeric(names(rle(first.choice)$values)) + (slice.size / 2) - (slice.overlap / 2)
        # replacing the last element with the text length
        end.segment[length(end.segment)] = entire.sample.length
        #zero.point = (slice.size / 2) - (slice.overlap / 2)
        zero.point = 0
        # use the final points, but get rid of the last one and add a zero.point at the front
        start.segment = c(zero.point, end.segment[1:(length(end.segment)-1)])



        # additional identifier for every single sample
        if(add.ticks == TRUE) {
          segments(as.numeric(names(classification.results)),0.14,as.numeric(names(classification.results)),0.12,
          col=colors.first.choice[classification.results] )
        }



        # strip I

        # identifying points where the style changes
        style.change.points = end.segment[1:(length(end.segment)-1)]
        # adding lines at each style break point
        segments(style.change.points,0.14,style.change.points,0)
        # drawing the strip: a sequence of colored rectangles
        rect(start.segment,0.15,end.segment,0.3,border=NA, col=colors.first.choice[rle(first.choice)$values])



        # strip II

        end.segment = as.numeric(names(rle(second.choice)$values)) + (slice.size / 2) - (slice.overlap / 2)
        end.segment[length(end.segment)] = entire.sample.length
        zero.point = 0
        start.segment = c(zero.point, end.segment[1:(length(end.segment)-1)])
        # drawing the strip: a sequence of colored rectangles
        rect(start.segment,0.31,end.segment,0.45,border=NA, col=colors.second.choice[rle(second.choice)$values])



        # strip III (for delta)
        if(classification.method == "delta" && length(attr(classification.results, "rankings")[1,]) > 2) {
                end.segment = as.numeric(names(rle(third.choice)$values)) + (slice.size / 2) - (slice.overlap / 2)
                end.segment[length(end.segment)] = entire.sample.length
                zero.point = 0
                start.segment = c(zero.point, end.segment[1:(length(end.segment)-1)])
                # drawing the strip: a sequence of colored rectangles
                rect(start.segment,0.46,end.segment,0.6,border=NA, col=colors.third.choice[rle(third.choice)$values])
        }



        if(classification.method == "svm" || classification.method == "nsc") {

                scores.raw = attr(classification.results, "scores")
                rownames(scores.raw) = names(classification.results)
                y = round(scores.raw, 1)

                # adjusting the svm decision values to the current scale {0,1}
                if(classification.method == "svm") {
                        y = (y + 1) / 2
                        y[y < 0.1] = 0
                        y[y > 1] = 1
                }

                # 1st candidate:

                end.segment = as.numeric(names(rle(y[,1])$values)) + (slice.size / 2) - (slice.size / 2)
                end.segment[length(end.segment)] = entire.sample.length
                zero.point = 0
                start.segment = c(zero.point, end.segment[1:(length(end.segment)-1)])
                # the area to be masked by a white rectangle, scaled to the stripe width
                masking.range = (1 - rle(y[,1])$values) * 0.15
                rect(start.segment,0.15,end.segment,masking.range+0.15,border=NA, col="white")

                # 2nd candidate:

                end.segment = as.numeric(names(rle(y[,2])$values)) + (slice.size / 2) - (slice.size / 2)
                end.segment[length(end.segment)] = entire.sample.length
                zero.point = 0
                start.segment = c(zero.point, end.segment[1:(length(end.segment)-1)])
                masking.range = (1 - rle(y[,2])$values) * 0.15
                rect(start.segment,0.46,end.segment,0.46-masking.range,border=NA,col="white")

        }


        # adding two vertical lines at the beginning and at the end
        abline(v=0, lty=2)
        abline(v=entire.sample.length, lty=2)


        # adding an optional legend on the right side of the plot
        if(plot.legend == TRUE) {
                legend(x = entire.sample.length, y = 0.95,
                       legend = names(colors.first.choice),
                       col = colors.first.choice,
                       bty = "n",
                       cex=0.75,
                       lwd = 5)
        }


        # adding optional ranking hints
        if(classification.method == "delta" && plot.legend == TRUE) {
                text(0, 0.225, expression(1^ st), adj = c(1.8,0.5))
                text(0, 0.38, expression(2^ nd), adj = c(1.5,0.5))
                text(0, 0.53, expression(3^ rd), adj = c(1.5,0.5))
        }
}




if(classification.method == "delta") {
  class.method = paste("delta-", distance.measure, sep="")
} else {
  class.method = classification.method
}



# check if a custom filename has been set
if(is.character(custom.graph.filename) == TRUE &
         length(custom.graph.filename) > 0) {
    # if a custom file name exists, then use it
    graph.filename = custom.graph.filename
} else {
  graph.filename = paste("rolling-", class.method, "_", mfw, "-features_",
                         slice.size, "-per-slice", sep="")
}





  if(display.on.screen == TRUE) {
    plot.current.task()
    }
  if(write.pdf.file == TRUE) {
    pdf(file = paste(graph.filename, ".pdf", sep=""),
            width=plot.custom.width,height=plot.custom.height,
            pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.jpg.file == TRUE) {
    jpeg(filename = paste(graph.filename, ".jpg", sep=""),
            width=plot.custom.width,height=plot.custom.height,
            units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.svg.file == TRUE) {
    svg(filename = paste(graph.filename, ".svg", sep=""),
            width=plot.custom.width,height=plot.custom.height,
            pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.png.file == TRUE) {
    png(filename = paste(graph.filename, ".png", sep=""),
            width=plot.custom.width,height=plot.custom.height,
            units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
#







# #################################################
# praparing final resutls: building a class




features.actually.used = colnames(training.set)
frequencies.training.set = freq.I.set.0.culling
frequencies.test.set = freq.II.set.0.culling

classification.rankings = attr(classification.results, "rankings")
classification.scores = attr(classification.results, "scores")






if(exists("classification.results")) {
  attr(classification.results, "description") = "classed assigned to particular test slices"
  class(classification.results) = "stylo.data"
}

if(exists("classification.rankings")) {
  attr(classification.rankings, "description") = "three first ranked candidates"
  class(classification.rankings) = "stylo.data"
}
if(exists("classification.scores")) {
  attr(classification.scores, "description") = "final scores of classification"
  class(classification.scores) = "stylo.data"
}
if(exists("features") & length(features) > 0 ) {
  attr(features, "description") = "features (e.g. words, n-grams, ...) applied to data"
  class(features) = "stylo.data"
}
if(exists("features.actually.used")) {
  attr(features.actually.used, "description") = "features (e.g. frequent words) actually analyzed"
  class(features.actually.used) = "stylo.data"
}
if(exists("frequencies.training.set")) {
  attr(frequencies.training.set, "description") = "frequencies of words/features in the training set"
  class(frequencies.training.set) = "stylo.data"
}
if(exists("frequencies.test.set")) {
  attr(frequencies.test.set, "description") = "frequencies of words/features in the test set"
  class(frequencies.test.set) = "stylo.data"
}


# creating an object (list) that will contain the final results,
# tables of frequencies, etc.etc.
results.rolling.classify = list()
# elements that we want to add on this list
variables.to.save = c("classification.scores",
                      "classification.results",
                      "classification.rankings",
                      "nearest.neighbors",
                      "features",
                      "milestone.points",
                      "text.length",
                      "features.actually.used",
                      "frequencies.training.set",
                      "frequencies.test.set")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.rolling.classify[[i]] = get(i)
}



# adding some information about the current function call
# to the final list of results
results.rolling.classify$call = match.call()
results.rolling.classify$name = call("rolling.classify")


# This assings the list of final resutls to the class "stylo.resutls";
# the same class will be used to handle the output of stylo(),
# rolling.delta() and oppose(). See the files "print.stylo.results.R"
# and "summary.stylo.results.R" (no help files are provided, since
# these two functions are not visible for the users).
class(results.rolling.classify) = "stylo.results"






# back to the original working directory
setwd(original.path)

# return the value of the function
return(results.rolling.classify)

}
