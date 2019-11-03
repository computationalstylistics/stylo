
# #################################################
# Function for applying a variety of multidimensional
# explanatory analyses to a collection of (literary) texts.
# To use this function, one needs to have a directory
# (it's name is customizable) containing some texts.
# Optional arguments: (1) GUI mode (default: TRUE),
# (2) path to your working directory, (3) name of
# the subdirectory containing the texts (default: "corpus")
# #################################################


stylo = function(gui = TRUE,
             frequencies = NULL,
             parsed.corpus = NULL,
             features = NULL,
             path = NULL,
             metadata = NULL,
             filename.column = "filename",
             grouping.column = "author",
             corpus.dir = "corpus", ...) {



# if any command-line arguments have been passed by a user, they will
# be stored on the following list and used to overwrite the defaults
passed.arguments = list(...)





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

if(is.character(corpus.dir) == FALSE | nchar(corpus.dir) == 0) {
  corpus.dir = "corpus"
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
        variables = gui.stylo(...)
      } else {
        message(" ")
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
mfw.incr = variables$mfw.incr
mfw.list.cutoff = variables$mfw.list.cutoff
mfw.max = variables$mfw.max
mfw.min = variables$mfw.min
ngram.size = variables$ngram.size
preserve.case = variables$preserve.case
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



# variables not available on GUI (yet)

# a. linkage algorighm
linkage = variables$linkage

# b. network analysis support
network = variables$network
network.tables = variables$network.tables
network.type = variables$network.type
linked.neighbors = variables$linked.neighbors
edge.weights = variables$edge.weights

# newly-added options
relative.frequencies = variables$relative.frequencies
splitting.rule = variables$splitting.rule
preserve.case = variables$preserve.case
encoding = variables$encoding
stop.words = variables$stop.words
sample.overlap = variables$sample.overlap
number.of.samples = variables$number.of.samples
custom.graph.filename = variables$custom.graph.filename


# #############################################################################
# Final settings (you are advised rather not to change them)
# #############################################################################

# If no language was chosen (or if a desired language is not supported, or if
# there was a spelling mistake), then the variable will be set to "English".

pronouns = stylo.pronouns(corpus.lang = corpus.lang)


# Since it it not so easy to perform, say, 17.9 iterations, or analyze
# 543.3 words, the code below rounds off all numerical variables to
# the nearest positive integers, to prevent you from making silly jokes
# with funny settings. (OK, it is still possible to crash the script in
# more ways than one, but you will have to find them on your own).

  mfw.min = round(mfw.min)
  mfw.max = round(mfw.max)
  mfw.incr = round(mfw.incr)
  start.at = round(start.at)
  culling.min = round(culling.min)
  culling.max = round(culling.max)
  culling.incr = round(culling.incr)
  mfw.list.cutoff = round(mfw.list.cutoff)
  sample.size = round(sample.size)


# resetting the default plot area (if an appropriate option has been chosen)
if(plot.options.reset == TRUE) {
  plot.custom.height = 7
  plot.custom.width = 7
  plot.font.size = 10
  plot.line.thickness = 1
  plot.options.reset = FALSE
  }



# If TXM compatibility mode has been chosen, other options need to be switched off
if(txm.compatibility.mode == TRUE) {
  # checking if a frequency table has been passed from TXM to R
  if(exists("txm.generated.freq.table") == TRUE) {
    # sanity check
    if(exists("variable.name") == FALSE) {
      variable.name = c()
      stop("TXM does not seem to pass any data to analyze")
    }
    # inheriting the table from TXM
    frequencies.0.culling = t(variable.name)
    # transposing the table
    frequencies.0.culling = frequencies.0.culling[-1,]
    # set the variable use.existing.freq.tables to skip uploading corpus files
    use.existing.freq.tables == TRUE
  } else {
     message("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Oops! To use TXM compatibility mode, you have to launch TXM first!\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    stop("Incorrect input data")
  }
}




###############################################################################
# Backward compatibility: if "use.existing.freq.tables" is switched on, then
# a file with a frequency table will be used, provided that it exists
  if(use.existing.freq.tables == TRUE
                            & file.exists("table_with_frequencies.txt") == TRUE ) {
    frequencies = "table_with_frequencies.txt"
  } else {
    use.existing.freq.tables = FALSE
  }
# Backward compatibility: if "use.existing.wordlist" is switched on, then
# the file "wordlist.txt" be used, provided that it does exist
  if(use.existing.wordlist == TRUE & file.exists("wordlist.txt") == TRUE ) {
    features = "wordlist.txt"
  } else {
    use.existing.wordlist = FALSE
  }
###############################################################################







# Network analysis: variable initialization
all.connections = 0





# Is custom title requested? If yes, use it
if(is.null(custom.graph.title) == FALSE) {
        # but first, a tiny sanitizing is needed
        graph.title = as.character(custom.graph.title)[1]
        graph.main.title = graph.title
} else {
        # otherwise, assign the current working directory name
        graph.title = basename(getwd())
        graph.main.title = graph.title
}




# no additional margin will be added on PCA/MDS plots, unless points & labels
# are switched to be shown together
if(text.id.on.graphs != "both") {
  label.offset = 0
  }


# if a chosen plot area is really large (and a bitmap output has been chosen),
# a warning will appear
if(write.jpg.file == TRUE || write.png.file == TRUE){
  # if the desired height*width (at 300 dpi) exceeds 36Mpx
  if(300*plot.custom.width * 300*plot.custom.height > 36000000) {
    cat("\nYou have chosen a bitmap output format and quite a large plot area\n")
    cat("of", plot.custom.width, "by", plot.custom.height, "inches. Producing some",
        as.integer(300*plot.custom.width * 300*plot.custom.height / 1000000),
        "Megapixels will take a good while.\n\n")
    cat("  i - ignore this warning and continue with the current settings\n")
    cat("  p - use pdf format instead of a bitmap (default)\n")
    cat("  s - shrink the plot area to a reasonable size of 20x20 inches\n")
    cat("  a - abort the script\n")
    # reading from the prompt
    answer = readline("\n[i/p/s/a]  ")
    if(tolower(answer) == "a") {
      stop("The script stopped by the user")
    } else if(tolower(answer) == "i") {
      cat("Okay (but honestly, do you really need such a large plot?)\n")
    } else if(tolower(answer) == "s") {
      cat("The plot area will be shrunken to 20x20 inches\n")
      plot.custom.width = 20
      plot.custom.height = 20
    } else {
      cat("Withdrawing from the bitmap output, performing pdf instead\n")
      write.jpg.file = FALSE
      write.svg.file = FALSE
      write.png.file = FALSE
      write.pdf.file = TRUE
    }
  }
}

# #############################################################################








###############################################################################
# Checking if the argument "features" has been used
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
        message("has a form of vector")
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
        message("\nreading a custom set of features from a file...")
        # reading a file: newlines are supposed to be delimiters
        features = scan(features,what="char",sep="\n",encoding=encoding)
        # getting rid of the lines beginning with the "#" char
        features = c(grep("^[^#]",features,value=TRUE))
        # link this vector into the variable used for calculations
        mfw.list.of.all = features
      } else {
        # if there's no such a file, then don't try to use it
        message("\n", "file \"",features, "\" could not be found")
        stop("Wrong file name")
      }
    # selecting the above vector as a valid set of features
    features.exist = TRUE
  }
###############################################################################





###############################################################################
# Checking if the argument "frequencies" has been used
# variable initialization:
corpus.exists = FALSE
#
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
        message("has a form of matrix/data frame")
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
        message("\n", "reading a file containing frequencies...")
        frequencies = t(read.table(frequencies, encoding=encoding))
      } else {
        # if there's no such a file, then don't try to use it
        message("\n", "file \"",frequencies, "\" could not be found")
        stop("Wrong file name")
      }
    # selecting the above matrix as a valid corpus
    corpus.exists = TRUE
  }


  # If a custom set of features was indicated, try to pick the matching variables only
  if(features.exist == TRUE & corpus.exists == TRUE) {
      # checking if the chosen features do match the columns of the table
      if(length(grep("TRUE",colnames(frequencies) %in% features)) < 2) {
        message("The features you want to analyze do not match the variables' names:\n")
        message("Available features: ",head(colnames(frequencies)), "...")
        message("Chosen features: ", head(features), "...")
        message("")
        message("Check the rotation of your table and the names of its rows and columns.")
        stop("Input data mismatch")
      } else {
        # if everything is right, select the subset of columns from the table:
        frequencies = frequencies[,colnames(frequencies) %in% features]
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
      message("\n")
      message("There is not enough samples and/or features to be analyzed.\n")
      message("Try to use tables of at least two rows by two columns.\n")
      message("\n")
      stop("Wrong size of the table of frequencies")
    }
  }
###############################################################################


if(corpus.exists == TRUE) {
  frequencies.0.culling = frequencies
}




# If the tables with frequencies could not loaded so far (for any reason), try to load
# an external corpus (R object) passed as an argument

###############################################################################
# Checking if the argument "parsed.corpus" has been used
#

  # checking if the variable "parsed.corpus" is empty
  if(corpus.exists == FALSE & length(parsed.corpus) > 0) {
      # if the variable was used, check its format
      if(is.list(parsed.corpus) == TRUE & length(parsed.corpus) > 1) {
          # checking if the samples have their names; otherwise, assign generic ones:
          if( length(names(parsed.corpus)) != length(parsed.corpus) ) {
            names(parsed.corpus) = paste("sample",1:length(parsed.corpus),sep="_")
          }
        # if everything is fine, use this variable as a valid corpus
        loaded.corpus = parsed.corpus
        message("Corpus loaded successfully.\n")
        corpus.exists = TRUE
      } else {
        message("\n")
        message("The object you've specified as your corpus cannot be used.\n")
        message("It should be a list containing particular text samples\n")
        message("(vectors containing sequencies of words/n-grams or other features).\n")
        message("The samples (elements of the list) should have their names.\n")
        message("Alternatively, try to build your corpus from text files (default).\n")
        message("\n")
        stop("Wrong corpus format")
      }
  }
###############################################################################




# If there's still no corpus available, then load and parse text files.
# They are supposed to be stored in a specified corpus subfolder and to follow
# a strictly defined naming convention.

###############################################################################
# Building a corpus from text files

if(corpus.exists == FALSE) {

  # Checking whether the required subdirectory exists, calling the choose directory dialogue if not.
  if(file.exists(corpus.dir) == FALSE) {
    selected.path = tk_choose.dir(caption = "Select your working directory. It should have a subdirectory called *corpus* ")
    setwd(selected.path)
  }
  if(file.exists(corpus.dir) == FALSE) {
          message("\n\n", "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
              "Hey! The working directory should contain the subdirectory \"",
              corpus.dir,"\"\n",
              "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
          # back to the original working directory
          setwd(original.path)
          # error message
          stop("Corpus prepared incorrectly")
  }
  
  # Retrieving the names of texts.
  # It's possible to choose the files manually (choose an appropriate option!)
  if (interactive.files == TRUE) {
    # go to corpus directory
    setwd(corpus.dir)
    corpus.filenames = basename(tk_choose.files(default = "",
                                caption = "Select at least 2 files", multi = TRUE))
    # back to the working directory
    setwd("..")
  } else {
    # alternatively, one can use the files listed in "files_to_analyze.txt";
    # the listed files can be separated by spaces, tabs, or newlines
      if(use.custom.list.of.files ==TRUE & file.exists("files_to_analyze.txt") ==TRUE) {
        # a message on the screen
        message("\n")
        message("external list of files will be used for uploading the corpus\n\n")
        # retrieving the filenames from a file
        corpus.filenames = scan("files_to_analyze.txt",
                                what="char",
                                sep="\n",
                                encoding=encoding,
                                quiet=T)
        # getting rid of spaces and/or tabs
        corpus.filenames = unlist(strsplit(corpus.filenames,"[ \t]+"))
          # checking whether all the files indicated on the list really exist
          if( length(setdiff(corpus.filenames,list.files(corpus.dir))) > 0 ){
            # if not, then sent a message and list the suspicious filenames
            message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
            message("the following files have not been found:\n")
            message(setdiff(corpus.filenames, list.files(corpus.dir)),"\n\n")
            message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
            # use only those files that match
            corpus.filenames = intersect(corpus.filenames, list.files(corpus.dir))
          }
      } else {
        # Generic solution: all the files from a specified directory will be used
        corpus.filenames = list.files(corpus.dir)
      }
  }

  # Checking whether the required files exist
    if(length(corpus.filenames) <2 & sampling != "normal.sampling")  {
      message("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
          "Ho! The subdirectory \"",corpus.dir,"\" should contain at least
          two text samples!\n",
          "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
      # back to the original working directory
      setwd(original.path)
      # error message
      stop("Corpus prepared incorrectly")
    }


  # some messages on the screen
    if (sampling == "normal.sampling"){
      message("Performing sampling (using sample size = ", sample.size,
            " words)\n")
    } else if(sampling == "random.sampling"){
      message("Performing random sampling (using random sample size = ",
            " words)\n")
    } else if (sampling == "no.sampling"){
      message("Performing no sampling (using entire text as sample)", "\n")
    } else {
      stop("Exception raised: something is wrong with the sampling parameter you have
            specified...")
    }



  # loading text files, splitting, parsing, n-gramming, samping, and so forth
  loaded.corpus = load.corpus.and.parse(files = corpus.filenames,
                         corpus.dir = corpus.dir,
                         encoding = encoding,
                         markup.type = corpus.format,
                         corpus.lang = corpus.lang,
                         splitting.rule = splitting.rule,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         sample.overlap = sample.overlap,
                         number.of.samples = number.of.samples,
                         features = analyzed.features,
                         ngram.size = ngram.size,
                         preserve.case = preserve.case)
}
###############################################################################



# At this point, a corpus SHOULD be available. If there's still no frequency
# table, it will be build at this stage

###############################################################################
# building the table of frequencies

if(exists("frequencies.0.culling") == FALSE) {

  # a message
  message("")
  message("Total nr. of samples in the corpus: ", length(loaded.corpus))


  # the directory with corpus must contain enough texts;
  # if the number of text samples is lower than 2, the script will abort.
  if( (length(loaded.corpus) < 2) & (sampling == "no.sampling") ) {
      message("\n\n","your corpus folder seems to be empty!", "\n\n")
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
    # Extracting all the words used in the corpus
    wordlist.of.loaded.corpus = c()
#wordlist.of.loaded.corpus = unlist(loaded.corpus)
    for (file in 1 : length(loaded.corpus)) {
      # loading the next sample from the list "corpus.filenames"
      current.text = loaded.corpus[[file]]
      # putting the files together:
      wordlist.of.loaded.corpus = c(wordlist.of.loaded.corpus, current.text)
      # short message on screen
      message(".", appendLF = FALSE)
      if(file/25 == floor(file/25)) { message("")} # a newline every 25th sample
    }

    # Preparing a sorted frequency list of the whole primary set (or both sets).
    # short message
    message("")
    message("The corpus consists of ", length(c(wordlist.of.loaded.corpus))," tokens")
    # the core procedure: frequency list
    mfw.list.of.all = sort(table(c(wordlist.of.loaded.corpus)),decreasing=T)
    # deleting the huge vector of all the words from the entire corpus
    rm(wordlist.of.loaded.corpus)
    # if the whole list is long, then cut off the tail, as specified in the GUI
    # by the cutoff value
      if (length(mfw.list.of.all) > mfw.list.cutoff) {
        mfw.list.of.all = mfw.list.of.all[1:mfw.list.cutoff]
      }
    # the only thing we need are words ordered by frequency (no frequencies)
    mfw.list.of.all = names(mfw.list.of.all)

    # Saving the list of features.
    # some comments into the file containing wordlist
    cat("# This file contains the words that were used for building the table",
      "# of frequencies. It can be also used for further tasks, and for this",
      "# purpose it can be manually revised, edited, deleted, culled, etc.",
      "# You can either delete unwanted words, or mark them with \"#\"",
      "# -----------------------------------------------------------------------",
      "", file="wordlist.txt", sep="\n")
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

  # blank line on the screen
  message("")

  # empty the dump-dir if it already existed and create it if it did not previously exist
  if(dump.samples == TRUE){
	  if (file.exists("sample_dump")){
		# a dump-dir seems to have been created during a previous run
		# tmp delete the dump-dir to remove all of its previous contents
		unlink("sample_dump", recursive = TRUE)
	  }
	# (re)create the dump-dir
	dir.create("sample_dump")
    # writing the stuff into files
    setwd("sample_dump")
      for(i in names(loaded.corpus)) {
        cat(loaded.corpus[[i]],file=paste(names(loaded.corpus[i]),".txt",sep=""))
      }
    setwd("..")
  }


  # preparing a huge table of all the frequencies for the whole corpus
  frequencies.0.culling = make.table.of.frequencies(corpus = loaded.corpus,
                                               features = mfw.list.of.all,
                                               relative = relative.frequencies)


  # writing the table with frequencies to a text file (it can be re-used!)
      if(encoding == "native.enc") {
        data.to.be.saved = t(frequencies.0.culling)
      } else {
        data.to.be.saved = t(frequencies.0.culling)
        rownames(data.to.be.saved) = iconv(rownames(data.to.be.saved), to=encoding)
        colnames(data.to.be.saved) = iconv(colnames(data.to.be.saved), to=encoding)
      }
  write.table(data.to.be.saved, file = "table_with_frequencies.txt")

}
###############################################################################
#
# #################################################
# the module for loading the corpus terminates here
# #################################################






# #################################################
# module for saving current config options
# #################################################

# Finally, we want to save some of the variable values for later use;
# they are automatically loaded into the GUI at the next run of the script.
cat("", file = "stylo_config.txt", append = FALSE)
var.name <- function(x) {
      if(is.character(x) == TRUE) {
        cat(paste(deparse(substitute(x))," = \"", x ,"\"", sep=""), file = "stylo_config.txt", sep = "\n", append = TRUE)
      } else {
        cat(paste(deparse(substitute(x)), x, sep = " = "), file = "stylo_config.txt", sep = "\n", append = TRUE) }
      }
var.name(corpus.format)
var.name(corpus.lang)
var.name(analyzed.features)
var.name(ngram.size)
var.name(preserve.case)
var.name(encoding)
var.name(mfw.min)
var.name(mfw.max)
var.name(mfw.incr)
var.name(start.at)
var.name(culling.min)
var.name(culling.max)
var.name(culling.incr)
var.name(mfw.list.cutoff)
var.name(delete.pronouns)
var.name(use.existing.freq.tables)
var.name(use.existing.wordlist)
var.name(use.custom.list.of.files)
var.name(analysis.type)
var.name(consensus.strength)
var.name(distance.measure)
var.name(sampling)
var.name(sample.size)
var.name(number.of.samples)
var.name(display.on.screen)
var.name(write.pdf.file)
var.name(write.jpg.file)
var.name(write.svg.file)
var.name(write.png.file)
var.name(plot.custom.height)
var.name(plot.custom.width)
var.name(plot.font.size)
var.name(plot.line.thickness)
var.name(text.id.on.graphs)
var.name(colors.on.graphs)
var.name(titles.on.graphs)
var.name(label.offset)
var.name(add.to.margins)
var.name(dendrogram.layout.horizontal)
var.name(pca.visual.flavour)
var.name(save.distance.tables)
var.name(save.analyzed.features)
var.name(save.analyzed.freqs)
var.name(dump.samples)

# #############################################################################









# #################################################
# MAIN PROGRAM; the main loop is below
# #################################################

# saving the original mfw.max value in mfw.max.original
# this is useful for graph subtitles
mfw.max.original = mfw.max

# the general counter for various purposes: initialization
number.of.current.iteration = 0

# load the ape library; make an empty bootstrap.results list
# this will be executed only if the bootstrap option is checked
if (analysis.type == "BCT") {
#    library(ape)
    bootstrap.list = list()
}


# #################################################
# module for culling (THE MAIN LOOP IN THE PROGRAM)
# #################################################


# testing if desired culling settings are acceptable;
# if too large, it is set to maximum possible
  if(culling.max >= 100) {
  culling.max = 100
  }
  if(culling.min >= 100) {
  culling.min = 100
  }
# if too small, it is set to 0 (i.e. minimal value)
  if(culling.min <= 0) {
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
        table.with.all.freqs = perform.culling(frequencies.0.culling,
                                        current.culling)


        # additionally, deleting pronouns (if applicable)
        if(delete.pronouns == TRUE) {
                table.with.all.freqs =
                delete.stop.words(table.with.all.freqs, pronouns)
        }


        # optionally, deleting stop words
        if(is.vector(stop.words) == TRUE) {
                table.with.all.freqs = delete.stop.words(table.with.all.freqs,
                                                         stop.words)
        }







# starting the frequency list at frequency rank set in option start.at above

# TO SAY THE TRUTH, IT CAN BE DONE MUCH EARLIER: at the moment when
# the frequency list for either I set or both sets is produced,
# it can be cut and used for building freq. tables

table.with.all.freqs = table.with.all.freqs[,start.at:length(table.with.all.freqs[1,])]









# Testing if the desired MFW number is acceptable,
# if MFW too large, it is set to maximum possible.
  if(mfw.max > length(table.with.all.freqs[1,])) {
  mfw.max = length(table.with.all.freqs[1,])
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



message("\n")
message("culling @ ", current.culling, "\t", "available features (words) ",
                  length(table.with.all.freqs[1,]))


# #################################################
# z-scores calcutations
# #################################################

if((analysis.type == "CA") || (analysis.type == "BCT") || (analysis.type == "MDS")){
  # calculating z-scores (a message on the screen)
  message("Calculating z-scores... \n")
  # Entropy distance: experimental, but entirely available yet
  # (the results do not really differ than for typical word frequencies)
  #
  #A = t(t(table.with.all.freqs + 1) / colSums(table.with.all.freqs + 1))
  #B =t(t(log(table.with.all.freqs + 2)) / -(colSums(A * log(A))))
  #table.with.all.freqs = B
  #
  # calculating z-scores
  table.with.all.zscores = scale(table.with.all.freqs)
  table.with.all.zscores = table.with.all.zscores[,]
}

# #################################################
# the internal loop starts here (for i = mfw.min : mfw.max)
# #################################################

# a short message on the screen about distance calculations (when appropriate):
if((analysis.type == "CA") || (analysis.type == "BCT") || (analysis.type == "MDS")){



# starting some variables that will be overwritten (unless a custom distance is used)
distance.name.on.graph = distance.measure
distance.name.on.file = distance.measure


  if(distance.measure == "delta" | distance.measure == "dist.delta") {
    message("Calculating classic Delta distances...")
    distance.name.on.graph = "Classic Delta distance"
    distance.name.on.file = "Classic Delta"
  } else if(distance.measure == "argamon" | distance.measure == "dist.argamon") {
    message("Calculating Argamon's Delta distances...")
    distance.name.on.graph = "Argamon's Delta distance"
    distance.name.on.file = "Argamon's Delta"
  } else if(distance.measure == "eder" |  distance.measure == "dist.eder") {
    message("Calculating Eder's Delta distances...")
    distance.name.on.graph = "Eder's Delta distance"
    distance.name.on.file = "Eder's Delta"
  } else if(distance.measure == "simple" | distance.measure == "dist.simple") {
    message("Calculating Eder's Simple distances...")
    distance.name.on.graph = "Eder's Simple distance"
    distance.name.on.file = "Eder's Simple"
  } else if(distance.measure == "manhattan" | distance.measure == "dist.manhattan") {
    message("Calculating Manhattan distances...")
    distance.name.on.graph = "Manhattan distance"
    distance.name.on.file = "Manhattan"
  } else if(distance.measure == "canberra" | distance.measure == "dist.canberra") {
    message("Calculating Canberra distances...")
    distance.name.on.graph = "Canberra distance"
    distance.name.on.file = "Canberra"
  } else if(distance.measure == "euclidean" | distance.measure == "dist.euclidean") {
    message("Calculating Euclidean distances...")
    distance.name.on.graph = "Euclidean distance"
    distance.name.on.file = "Euclidean"
  } else if(distance.measure == "cosine" | distance.measure == "dist.cosine") {
    message("Calculating Cosine distances...")
    distance.name.on.graph = "Cosine distance"
    distance.name.on.file = "Cosine"
  } else {
    distance.name.on.graph = paste("Distance:", distance.measure)
    distance.name.on.file = distance.measure
  }

}




message("MFW used: ")

for(i in seq(mfw.min,mfw.max,round(mfw.incr)) ) {
mfw = i


# for safety reasons, if MFWs > variables in samples
if(mfw > length(colnames(table.with.all.freqs)) ) {
  mfw = length(colnames(table.with.all.freqs))
}

# the general counter for various purposes
number.of.current.iteration = number.of.current.iteration + 1

# the current task (number of MFW currently analyzed) echoed on the screen
message(mfw, " ", appendLF = FALSE)

# #################################################
# module for calculating distances between texts
# #################################################

if((analysis.type == "CA") || (analysis.type == "BCT") || (analysis.type == "MDS")){

input.freq.table = table.with.all.freqs[,1:mfw]


supported.measures = c("dist.euclidean", "dist.manhattan", "dist.canberra",
                       "dist.delta", "dist.eder", "dist.argamon",
                       "dist.simple", "dist.cosine", "dist.wurzburg",
                       "dist.entropy", "dist.minmax")



# if the requested distance name is confusing, stop
if(length(grep(distance.measure, supported.measures)) > 1 ) {
    stop("Ambiguous distance method: which one did you want to use, really?")

# if the requested distance name was not found invoke a custom plugin
} else if(length(grep(distance.measure, supported.measures)) == 0 ){

    # first, check if a requested custom function exists
    if(is.function(get(distance.measure)) == TRUE) {
        # if OK, then use the value of the variable 'distance.measure' to invoke
        # the function of the same name, with x as its argument
        distance.table = do.call(distance.measure, list(x = input.freq.table))
        # check if the invoked function did produce a distance
        if(class(distance.table) != "dist") {
            # say something nasty here, if it didn't:
            stop("it wasn't a real distance measure function applied, was it?")
        }
    }

# when the chosen distance measure is among the supported ones, use it
} else {

    # extract the long name of the distance (the "official" name)
    distance = supported.measures[grep(distance.measure, supported.measures)]
    # then check if this is one of standard methods supported by dist()
    if(distance %in% c("dist.manhattan", "dist.euclidean", "dist.canberra")) {
         # get rid of the "dist." in the distance name
         distance = gsub("dist.", "", distance)
         # apply a standard distance, using the generic dist() function
         distance.table = as.matrix(dist(input.freq.table, method = distance))
    # then, check for the non-standard methods but still supported by Stylo
    } else if(distance %in% c("dist.simple", "dist.cosine", "dist.entropy", "dist.minmax")) {

         # invoke one of the distance measures functions from Stylo
         distance.table = do.call(distance, list(x = input.freq.table[,1:mfw]))

    } else if(distance == "dist.wurzburg") {

         # invoke one of the distance measures functions from Stylo
         distance.table = do.call(distance, list(x = table.with.all.zscores[,1:mfw]))

    } else {
         # invoke one of the distances supported by 'stylo'; this is slightly
         # different from the custom functions invoked above, since it uses
         # another argument: z-scores can be calculated outside of the function
         distance.table = do.call(distance, list(x = table.with.all.zscores[,1:mfw], scale = FALSE))
    }

}

# convert the table to the format of matrix
distance.table = as.matrix(distance.table)



  # replaces the names of the samples (the extension ".txt" is cut off)
  rownames(distance.table)=gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)",
                        "",rownames(table.with.all.freqs))
  colnames(distance.table)=gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)",
                        "",rownames(table.with.all.freqs))
}





# #################################################
# a tiny module for graph auto-coloring:
# uses the functions "metadata.processing()"
# and assign.plot.colors()"
# #################################################

groups = process.metadata(metadata = metadata, 
                          filenames = rownames(table.with.all.freqs),
                          filename.column = filename.column,
                          grouping.column = grouping.column)

# using an appropriate function to assing colors to subsequent samples
colors.of.pca.graph = assign.plot.colors(labels = groups,
                            col = colors.on.graphs, opacity = 1)





# #################################################
# preparing the graphs
# #################################################

# The name of a given method will appear in the title of the graph
# (if the appropriate option was chosen), and will be pasted into
# a filename of the current job. First, variables are initiated...
name.of.the.method = ""
short.name.of.the.method = ""
mfw.info = mfw
plot.current.task = function() {NULL}

# getting rid of redundant start.at information
  if(start.at == 1) {
    start.at.info = ""
    } else {
    start.at.info = paste("Started at",start.at) }
# getting rid of redundant pronoun information
  if(delete.pronouns == TRUE) {
    pronouns.info = paste("Pronouns deleted")
    } else {
    pronouns.info = "" }
# getting rid of redundant culling information
  if(culling.min == culling.max) {
    culling.info = culling.min
    } else {
    culling.info = paste(culling.min, "-", culling.max, sep = "") }

# prepares a dendrogram for the current MFW value for CA plotting
if(analysis.type == "CA") {
  name.of.the.method = "Cluster Analysis"
  short.name.of.the.method = "CA"
  if(dendrogram.layout.horizontal == TRUE) {
    dendrogram.margins =  c(5,4,4,8)+0.1
    } else {
    dendrogram.margins = c(8,5,4,4)+0.1 }
  # the following task will be plotted
  plot.current.task = function(){
    par(mar=dendrogram.margins)
        # neighbor joining clustering algorithm needs a different call:
        if(linkage == "nj") {
          plot(nj(distance.table), font=1, tip.color = colors.of.pca.graph)
        # any other linkage algorithm is produced by hclust()
        } else {
          # clustering the distances stored in the distance.table
          clustered.data = hclust(as.dist(distance.table), method = linkage)
          # reordering the vector of colors to fit the order of clusters
          colors.on.dendrogram = colors.of.pca.graph[clustered.data$order]
          # converting the clusters into common dendrogram format
          tree.with.clusters = as.dendrogram(clustered.data, hang=0)
          # now, preparing the procedure for changing leaves' color attributes
          # (this snippet is taken from "help(dendrapply)" and slightly adjusted)
                  colLab = function(n) {
                          if(is.leaf(n)) {
                                  a <- attributes(n)
                                  i <<- i+1
                                  attr(n, "nodePar") <-
                                  c(a$nodePar, lab.col = mycols[i], pch = NA)
                          }
                          n
                  }
                  mycols = colors.on.dendrogram
                  attributes(mycols) = NULL
                  i = 0
          # adding the attributes to subsequent leaves of the dendrogram,
          # using the above colLab(n) function
          dendrogram.with.colors = dendrapply(tree.with.clusters, colLab)
          # finally, ploting the whole stuff
          plot(dendrogram.with.colors, main = graph.main.title,
                  horiz = dendrogram.layout.horizontal)
          if(dendrogram.layout.horizontal == TRUE) {
                  title(sub = graph.subtitle)
          } else {
                  title(sub = graph.subtitle, outer = TRUE, line = -1)
          }
        }
    }
}


# prepares a 2-dimensional plot (MDS) for plotting
if(analysis.type == "MDS") {
  name.of.the.method = "Multidimensional Scaling"
  distance.name.on.graph = ""
  distance.name.on.file = ""
  short.name.of.the.method = "MDS"
  mds.results = cmdscale(distance.table, eig = TRUE)
  # prepare the xy coordinates, add the margins, add the label offset
  xy.coord = mds.results$points[,1:2]
  if(text.id.on.graphs == "both") {
    label.coord = cbind(mds.results$points[,1], (mds.results$points[,2] + (0.01*label.offset*
                      abs(max(mds.results$points[,2]) - min(mds.results$points[,2])))))
    } else {
    label.coord = xy.coord
    }
  plot.area = define.plot.area(mds.results$points[,1], mds.results$points[,2],
                               xymargins = add.to.margins,
                               v.offset = label.offset)
  # define the plotting function needed:
  plot.current.task = function(){
    if(text.id.on.graphs == "points" || text.id.on.graphs == "both") {
      plot(xy.coord, type = "p",
           ylab = "", xlab = "",
           xlim = plot.area[[1]], ylim = plot.area[[2]],
           main = graph.main.title,
           sub = graph.subtitle,
           col = colors.of.pca.graph,
           lwd = plot.line.thickness)
      }
    if(text.id.on.graphs == "labels") {
      plot(xy.coord, type = "n",
           ylab = "", xlab = "",
           xlim = plot.area[[1]], ylim = plot.area[[2]],
           main = graph.main.title,
           sub = graph.subtitle,
           col = colors.of.pca.graph,
           lwd = plot.line.thickness)
      }
    if(text.id.on.graphs == "labels" || text.id.on.graphs == "both") {
      text(label.coord, rownames(label.coord), col=colors.of.pca.graph)
      }
    axis(1, lwd = plot.line.thickness)
    axis(2, lwd = plot.line.thickness)
    box(lwd = plot.line.thickness)
  }
}

if(analysis.type == "tSNE") {
    # set some string information variables
    name.of.the.method = "t-Distributed Stochastic Neighbor Embedding"
    short.name.of.the.method = "t-SNE"
    distance.name.on.file = "tSNE"
    distance.name.on.graph = "t-SNE"
    plot.current.task = function(){
        ecb = function(x,y){
            if(titles.on.graphs == TRUE) {
                graph.main.title = paste(graph.title,"\nt-SNE visualisation")
            } else {
                graph.main.title = ""
            }
            plot(x, t='n', main = graph.main.title, xlab = "", ylab = "", yaxt = "n", xaxt = "n")
            text(x, rownames(table.with.all.freqs[,1:mfw]), cex = 0.3)
        }
    tsne(X = table.with.all.freqs[,1:mfw], initial_dims = 50, epoch_callback = ecb, perplexity = 50, max_iter = 2000)
    }
}

# prepares Principal Components Analysis (PCA) for plotting
if(analysis.type == "PCV" || analysis.type == "PCR") {
  # set some string information variables
  name.of.the.method = "Principal Components Analysis"
  short.name.of.the.method = "PCA"
  distance.name.on.file = "PCA"
  if(analysis.type == "PCV") {
    pca.results = prcomp(table.with.all.freqs[,1:mfw])
    distance.name.on.graph = "Covariance matrix"
  } else if(analysis.type == "PCR") {
    pca.results = prcomp(table.with.all.freqs[,1:mfw], scale=TRUE)
    distance.name.on.graph = "Correlation matrix"
  }
  # get the variation explained by the PCs:
  expl.var = round(((pca.results$sdev^2) / sum(pca.results$sdev^2) * 100), 1)
  PC1_lab = paste("PC1 (", expl.var[1], "%)", sep="")
  PC2_lab = paste("PC2 (", expl.var[2], "%)", sep="")

  # prepare the xy coordinates, add the margins, add the label offset
  xy.coord = pca.results$x[,1:2]
  if(text.id.on.graphs == "both") {
    label.coord = cbind(pca.results$x[,1],(pca.results$x[,2] + (0.01*label.offset*
                      abs(max(pca.results$x[,2]) - min(pca.results$x[,2])))))
    } else {
    label.coord = xy.coord
    }
  plot.area = define.plot.area(pca.results$x[,1], pca.results$x[,2],
                               xymargins = add.to.margins,
                               v.offset = label.offset)
  # define the plotting function needed:
  plot.current.task = function(){
    if (pca.visual.flavour == "classic"){
      if(text.id.on.graphs == "points" || text.id.on.graphs == "both") {
        plot(xy.coord,
             type = "p",
             xlim = plot.area[[1]], ylim = plot.area[[2]],
             xlab = "", ylab = PC2_lab,
             main = graph.main.title, sub = paste(PC1_lab, "\n", graph.subtitle),
             col = colors.of.pca.graph,
             lwd = plot.line.thickness)
      }
      if(text.id.on.graphs == "labels") {
        plot(xy.coord,
             type = "n",
             xlim = plot.area[[1]], ylim = plot.area[[2]],
             xlab = "", ylab = PC2_lab,
             main = graph.main.title, sub = paste(PC1_lab, "\n", graph.subtitle),
             col = colors.of.pca.graph,
             lwd = plot.line.thickness)
      }
      abline(h=0, v=0, col = "gray60",lty=2)
      if(text.id.on.graphs == "labels" || text.id.on.graphs == "both") {
        text(label.coord, rownames(pca.results$x), col = colors.of.pca.graph)
      }
      axis(1, lwd = plot.line.thickness)
      axis(2, lwd = plot.line.thickness)
      box(lwd = plot.line.thickness)
    } else if(pca.visual.flavour == "loadings"){
      biplot(pca.results,
             col=c("grey70", "black"),
             cex=c(0.7, 1), xlab = "",
             ylab = PC2_lab,
             main = paste(graph.main.title, "\n\n", sep=""),
             sub = paste(PC1_lab, "\n", graph.subtitle, sep=""), var.axes = FALSE)
    } else if(pca.visual.flavour == "technical"){
      layout(matrix(c(1,2), 2, 2, byrow = TRUE), widths=c(3,1))
      biplot(pca.results, col=c("black", "grey40"), cex=c(1, 0.9), xlab="", ylab=PC2_lab, main=paste(graph.main.title, "\n\n", sep=""), sub=paste(PC1_lab,"\n",graph.subtitle, sep=""),var.axes=FALSE)
      abline(h=0, v=0, col = "gray60",lty=3)
      # add the subpanel to the right
      row = mat.or.vec(nc = ncol(pca.results$x), nr = 1)
      for (i in 1:ncol(row)){row[,i] = "grey45"}
      # paint the first two PCS black -- i.e. the ones actually plotted
      row[,1] = "black"
      row[,2] = "black"
      barplot(expl.var, col = row, xlab = "Principal components", ylab = "Proportion of variance explained (in %)")
      # set a horizontal dashed line, indicating the psychological 5% barrier
      abline(h = 5, lty = 3)
    } else if(pca.visual.flavour == "symbols"){
      # determine labels involved
      labels = c()
      for (c in rownames(pca.results$x)){
        labels = c(labels, gsub("_.*", "", c))
      }
      COOR = data.frame(pca.results$x[,1:2], LABEL = labels)
      labels = c(levels(COOR$LABEL))
      # visualize
      sps = trellis.par.get("superpose.symbol")
      sps$pch = 1:length(labels)
      trellis.par.set("superpose.symbol", sps)
      ltheme = canonical.theme(color = FALSE)
      lattice.options(default.theme = ltheme)
      pl = xyplot(data = COOR, x = PC2~PC1, xlab = paste(PC1_lab, "\n", graph.subtitle, sep = ""), ylab = PC2_lab, groups = COOR$LABEL, sub = "", key = list(columns = 2, text = list(labels), points = Rows(sps, 1:length(labels))),
             panel = function(x, ...){
                 panel.xyplot(x, ...)
                 panel.abline(v = 0, lty = 3)
                 panel.abline(h = 0, lty = 3)
             })
      plot(pl)
    }
  }
}


# prepares a list of dendrogram-like structures for a bootstrap consensus tree
# (the final tree will be generated later, outside the main loop of the script)
if (analysis.type == "BCT") {
  mfw.info = paste(mfw.min, "-", mfw.info, sep = "")
  name.of.the.method = "Bootstrap Consensus Tree"
  short.name.of.the.method = "Consensus"
  # calculates the dendrogram for current settings
  #
########################################################################
########################################################################
# compatibility mode: to make one's old experiments reproducible
  if(linkage == "nj") {
    current.bootstrap.results = nj(as.dist(distance.table))
    } else {
    current.bootstrap.results = as.phylo(hclust(as.dist(distance.table),
                                       method = linkage))
  }
########################################################################
  # adds the current dendrogram to the list of all dendrograms
  bootstrap.list[[number.of.current.iteration]] = current.bootstrap.results }


# establishing the text to appear on the graph (unless "notitle" was chosen)
if(ngram.size > 1) {
      ngram.value = paste(ngram.size, "-grams", sep="")
  } else {
      ngram.value = ""
  }
  #
if(titles.on.graphs == TRUE) {
  graph.main.title = paste(graph.title, "\n", name.of.the.method)
  if(analysis.type == "BCT") {
      graph.subtitle = paste(mfw.info," MF",toupper(analyzed.features)," ",ngram.value," Culled @ ",culling.info,"%\n",
                    pronouns.info," ",distance.name.on.graph," Consensus ",consensus.strength," ",start.at.info, sep="")
  } else {
      graph.subtitle = paste(mfw.info," MF",toupper(analyzed.features)," ",ngram.value," Culled @ ",culling.info,"%\n",
      pronouns.info," ",distance.name.on.graph," ",start.at.info, sep="") }
  } else {
  graph.main.title = ""
  graph.subtitle = "" }


# name of the output file (strictly speaking: basename) for graphs

# check if a custom filename has been set
if(is.character(custom.graph.filename) == TRUE &
         length(custom.graph.filename) > 0) {
    # if a custom file name exists, then use it
    graph.filename = custom.graph.filename
} else {
  # otherwise, combine some information into the filename
    if(analysis.type == "BCT") {
        graph.filename = paste(basename(getwd()), short.name.of.the.method,
                         mfw.info, "MFWs_Culled", culling.info,pronouns.info,
                         distance.name.on.file, "C", consensus.strength,
                         start.at.info, sep="_")
    } else {
        graph.filename = paste(basename(getwd()), short.name.of.the.method,
                         mfw.info, "MFWs_Culled", culling.info,pronouns.info,
                         distance.name.on.file, start.at.info, sep = "_")
    }
}


# #################################################
# plotting
# #################################################

# The core code for the graphic output (if bootstrap consensus tree
# is specified, the plot will be initiated later)
if(analysis.type != "BCT") {
  if(display.on.screen == TRUE) {
    plot.current.task()
    }
  if(write.pdf.file == TRUE) {
    pdf(file = paste(graph.filename,"_%03d",".pdf",sep=""),
            width=plot.custom.width,height=plot.custom.height,
            pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.jpg.file == TRUE) {
    jpeg(filename = paste(graph.filename,"_%03d",".jpg",sep=""),
            width=plot.custom.width,height=plot.custom.height,
            units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.svg.file == TRUE) {
    svg(filename = paste(graph.filename,"_%03d",".svg",sep=""),
            width=plot.custom.width,height=plot.custom.height,
            pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.png.file == TRUE) {
    png(filename = paste(graph.filename,"_%03d",".png",sep=""),
            width=plot.custom.width,height=plot.custom.height,
            units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
}
##################################################


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
features.actually.used = colnames(table.with.all.freqs[,1:mfw])
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
      data.to.be.saved = t(table.with.all.freqs[,1:mfw])
    } else {
      data.to.be.saved = t(table.with.all.freqs[,1:mfw])
      rownames(data.to.be.saved) = iconv(rownames(data.to.be.saved), to = encoding)
      colnames(data.to.be.saved) = iconv(colnames(data.to.be.saved), to = encoding)
    }
  # writting the stuff -- the file name will be changed accordingly
  write.table(data.to.be.saved,
     file = paste("frequencies_analyzed_", mfw, "mfw_", current.culling, "c.txt", sep = ""))
}





##############################################
##############################################
# network analysis, stage I: preparing edges/nodes
##############################################

if((exists("distance.table") == TRUE) & (network == TRUE)) {
  distances = distance.table
  # next, we need to create an empty matrix of the same size as the dist table
  connections = matrix(data = 0, nrow = length(distances[,1]), ncol = length(distances[1,]))
  # iterate over the rows of dist matrix to retrieve subsequent nearest neighbors
  for(i in 1: length(distances[,1])) {
    # establish a link between two nearest neighbors by assigning 3,
    # 2nd runner-up will get 2, and 3rd runner-up will get 1
    # original implementation:
    #connections[i,(order(distances[i,])[2])] = 3
    #connections[i,(order(distances[i,])[3])] = 2
    #connections[i,(order(distances[i,])[4])] = 1
    #
    for(k in 1:linked.neighbors) {
      connections[i,(order(distances[i,])[k+1])] = linked.neighbors - k + 1
    }
  }
  # optionally, apply a transformation function to the links' weights
  if(edge.weights == "quadratic") {
    connections = connections^2
  } else if(edge.weights == "log") {
    connections = log(connections +1)
  }
all.connections = all.connections + connections
}
##############################################
##############################################







}    # <-- the internal loop for(i) returns here
# #################################################

# blank line on the screen
message(" ")


}    # <-- the main loop for(j) returns here
# ################################################





######################################################
######################################################
# network analysis, stage II: preparing a list of edges

if((exists("distance.table") == TRUE) & (network == TRUE)) {
  rownames(all.connections) = rownames(distances)
  colnames(all.connections) = colnames(distances)

  if(network.tables == "edges") {
    # only one table (list of edges) will be created;
    # the simplest way to get a network in Gephi
    edges = c()
    for(i in 1:(length(all.connections[,1])) ) {
      for(j in 1:(length(all.connections[1,])) ) {
        from = rownames(all.connections)[i]
        to = colnames(all.connections)[j]
        if(network.type == "undirected") {
          # undirected, i.e. links "to" and "from" are summarized;
          # it means that possible bias is partialy overcome
          weight = all.connections[i,j] + all.connections[j,i]
          current.row = c(from, to, weight, "undirected")
        } else {
          # directed: it matters whether a given sample points or is poited
          weight = all.connections[i,j]
          current.row = c(from, to, weight, "directed")
        }
        # if there is a connection, record it in a common table
        if(weight > 0) {
          edges = rbind(edges, current.row)
        }
      }
    }
    #
    # assigning column names and row names
    colnames(edges) = c("Source", "Target", "Weight", "Type")
    rownames(edges) = c(1:length(edges[,1]))
    # for some reason, the table has to be explicitly declared
    edges = as.data.frame(edges)
    # preparing a file name
    edges.filename = paste(graph.filename, "EDGES.csv", sep = "")
    # writing to a file
    write.csv(edges, file = edges.filename, quote = FALSE, row.names = FALSE)
    #
    #
  } else {
    # two tables (list of edges, list of nodes) will be created;
    # this can be used with Gephi, and it is potentially more flexible
    edges = c()
    for(i in 1:(length(all.connections[,1])) ) {
      for(j in 1:(length(all.connections[1,])) ) {
        from = c(1:length(rownames(all.connections)))[i]
        to = c(1:length(colnames(all.connections)))[j]
        if(network.type == "undirected") {
          # undirected, i.e. links "to" and "from" are summarized;
          # it means that possible bias is partialy overcome
          weight = all.connections[i,j] + all.connections[j,i]
          # a trick to start counting naming the nodes from 0
          current.row = c(from -1, to -1, weight, "undirected")
        } else {
          # directed: it matters whether a given sample points or is poited
          weight = all.connections[i,j]
          # a trick to start counting naming the nodes from 0
          current.row = c(from -1, to -1, weight, "directed")
        }
        # if there is a connection, record it in a common table
        if(weight > 0) {
          edges = rbind(edges, current.row)
        }
      }
    }
    #
    # assigning column names and row names
    colnames(edges) = c("Source", "Target", "Weight", "Type")
    rownames(edges) = c(1:length(edges[,1]))
    # for some reason, the table has to be explicitly declared
    edges = as.data.frame(edges, stringsAsFactors = FALSE)
    #
    # preparing the table of nodes
    node.id = c(1:length(rownames(all.connections))) -1
    node.names = rownames(all.connections)
    node.classes = gsub("_.*","",node.names)
    node.classes.numeric = as.numeric(factor(gsub("_.*", "", node.names)))
    nodes = cbind(node.id, node.names, node.classes, node.classes.numeric)
    colnames(nodes) = c("Id", "Label", "Classes", "Group")
    nodes = as.data.frame(nodes, stringsAsFactors = FALSE)
    #
    # preparing a file name (edges)
    edges.filename = paste(graph.filename, "EDGES.csv", sep = "")
    # writing to a file (edges)
    write.csv(edges, file = edges.filename, quote = FALSE, row.names = FALSE)
    # preparing a file name (nodes)
    nodes.filename = paste(graph.filename, "NODES.csv", sep = "")
    # writing to a file (nodes)
    write.csv(nodes, file = nodes.filename, quote = FALSE, row.names = FALSE)
  }
}

# finally, removing the network if it does not really exist
if(length(all.connections) == 1) {
 rm(all.connections)
}
######################################################
######################################################




# bootstrap visualization
if(analysis.type == "BCT") {

# as above, the task to be plotted is saved as a function
if(length(bootstrap.list) <= 2) {
  message("\n\nSORRY, BUT YOU ARE EXPECTING TOO MUCH...!\n",
  "There should be at least 3 iterations to make a consensus tree\n")
  } else {
  plot.current.task = function(){
        plot(consensus(bootstrap.list, p=consensus.strength),
           type="u",
           font=1,
           lab4ut="axial",
           tip.color = colors.of.pca.graph)
        title (main = graph.main.title)
        title (sub = graph.subtitle) }

# The core code for the graphic output... Yes, you are right: you've seen
# the same lines above. Instead of blaming us, write better code yourself
# and let us know.
  if(display.on.screen == TRUE) {
    plot.current.task()
    }
  if(write.pdf.file == TRUE) {
    pdf(file = paste(graph.filename,"_%03d",".pdf",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.jpg.file == TRUE) {
    jpeg(filename = paste(graph.filename,"_%03d",".jpg",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.svg.file == TRUE) {
    svg(filename=paste(graph.filename,"_%03d",".svg",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.png.file == TRUE) {
    png(filename = paste(graph.filename,"_%03d",".png",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
}}






# #################################################
# praparing final resutls: building a class



# something's wrong with the variable "features"; needs to be corrected above
features = mfw.list.of.all
# just to give it a more comprehensive name:
if(exists("pca.results") == TRUE ) {
  pca.coordinates = pca.results$x
  pca.rotation = pca.results$rotation
  pca.sdev = pca.results$sdev
  pca.var.exp = round((pca.results$sdev^2) / sum(pca.results$sdev^2) *100, 2)
}
if(exists("all.connections") == TRUE ) {
  table.edges = all.connections
}
if(exists("edges") == TRUE & length(edges) >1) {
  list.of.edges = edges
}
if(exists("nodes") == TRUE ) {
  list.of.nodes = nodes
}










if(exists("distance.table")) {
  attr(distance.table, "description") = "final distances between each pair of samples"
  class(distance.table) = c("stylo.data", "matrix")
}
if(exists("frequencies.0.culling")) {
  attr(frequencies.0.culling, "description") = "frequencies of words/features accross the corpus"
  class(frequencies.0.culling) = c("stylo.data", "matrix")
}
if(exists("table.with.all.freqs")) {
  attr(table.with.all.freqs, "description") = "frequencies of words/features accross the corpus"
  class(table.with.all.freqs) = c("stylo.data", "matrix")
}
if(exists("table.with.all.zscores")) {
  attr(table.with.all.zscores, "description") = "z-scored frequencies accross the corpus"
  class(table.with.all.zscores) = c("stylo.data", "matrix")
}
if(exists("features")) {
  attr(features, "description") = "features (e.g. words, n-grams, ...) applied to data"
  class(features) = "stylo.data"
}
if(exists("features.actually.used")) {
  attr(features.actually.used, "description") = "features (e.g. frequent words) actually analyzed"
  class(features.actually.used) = "stylo.data"
}
if(exists("table.of.edges")) {
  attr(table.of.edges, "description") = "edges of a network of stylometric similarities"
# this does not work:
#  class(table.of.edges) = "stylo.data"
}
if(exists("list.of.edges")) {
  attr(list.of.edges, "description") = "edges of a network of stylometric similarities"
# this does not work:
#  class(list.of.edges) = "stylo.data"
}
if(exists("list.of.nodes")) {
  attr(list.of.nodes, "description") = "nodes of a network of stylometric similarities"
# this does not work:
#  class(list.of.nodes) = "stylo.data"
}
if(exists("pca.coordinates")) {
  attr(pca.coordinates, "description") = "PCA matrix of coordinates for particular PCs"
  class(pca.coordinates) = c("stylo.data", "matrix")
}
if(exists("pca.rotation")) {
  attr(pca.rotation, "description") = "PCA matrix of variable loadings' eigenvectors"
  class(pca.rotation) = c("stylo.data", "matrix")
}
if(exists("pca.sdev")) {
  attr(pca.sdev, "description") = "PCA: standard deviations or particular PCs"
  class(pca.sdev) = c("stylo.data", "matrix")
}
if(exists("pca.var.exp")) {
  attr(pca.var.exp, "description") = "PCA: explained variance [%] for particular PCs"
  class(pca.var.exp) = c("stylo.data", "matrix")
}






# creating an object (list) that will contain the final results,
# tables of frequencies, etc.etc.
# This list will be turned into the class "styloresults"
results.stylo = list()
# elements that we want to add on this list
variables.to.save = c("distance.table",
                      "frequencies.0.culling",
                      "table.with.all.freqs",
                      "table.with.all.zscores",
                      "features",
                      "features.actually.used",
                      "pca.coordinates",
                      "pca.rotation",
                      "pca.sdev",
                      "pca.var.exp",
                      "table.of.edges",
                      "list.of.edges",
                      "list.of.nodes")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.stylo[[i]] = get(i)
}



# adding some information about the current function call
# to the final list of results
results.stylo$call = match.call()
results.stylo$name = call("stylo")


# This assings the list of final resutls to the class "stylo.resutls";
# the same class will be used to handle the output of classify(),
# rolling.delta() and oppose(). See the files "print.stylo.results.R"
# and "summary.stylo.results.R" (no help files are provided, since
# these two functions are not visible for the users).
class(results.stylo) = "stylo.results"



# back to the original working directory
setwd(original.path)


# return the value of the function
return(results.stylo)
}
