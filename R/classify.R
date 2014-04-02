
# Function that performs a number of machine-learning methods
# of classification used in computational stylistics: Delta (Burrows, 2002), 
# k-Nearest Neighbors classification, Support Vectors Machines, Naive Bayes, 
# and Nearest Shrunken Centroids (Jockers and Witten, 2010). Most of the options 
# are derived from the 'stylo' function.

classify <-
function(gui = TRUE,
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
  cat("using current directory...\n")
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
  variables = gui.classify(...)
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
length.of.random.sample = variables$length.of.random.sample
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

cv = variables$cv
cv.folds = variables$cv.folds









# #############################################################################
# Final settings (you are advised rather not to change them)
# #############################################################################


# Given a language option ("English", "Polish", "Latin" etc., as described 
# above), this procedure selects one of the lists of pronouns
# If no language was chosen (or if a desired language is not supported, or if 
# there was a spelling mistake), then the variable will be set to "English". 

pronouns = stylo.pronouns(language=corpus.lang)


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
# the file "wordlist.txt" be used, provided that it does exist
  if(use.existing.wordlist == TRUE & file.exists("wordlist.txt") == TRUE ) { 
    features = "wordlist.txt"
  } else {
    use.existing.wordlist = FALSE
  }
###############################################################################



# #############################################################################
# #############################################################################






# #################################################
# FUNCTIONS:







# #################################################
# Function for preparing and printing
# the final ranking of the least unlikely authors
# for each text in a given table of distances.
# Arguments: 1. table with distances, 2. number of candidates to be displayed
# #################################################

# what about getting back to this old version in which ALL candidates
# were listed, instead of listing only those misclassified?
# it can be done as an option

make.ranking.of.candidates = function(dist.matrix,candidates) {
  for(h in 1:length(dist.matrix[,1])) {
  ranked.candidates = order(dist.matrix[h,])
  current.sample = c(gsub("_.*","",colnames(dist.matrix)))[ranked.candidates]
    if((gsub("_.*","",rownames(dist.matrix)))[h] != current.sample[1]) {
  	cat(c(c(rownames(dist.matrix))[h]," ", "-->", " ", 
      current.sample[1:candidates], " (delta score:", 
      round(dist.matrix[h,ranked.candidates[1:candidates]],4), ")",
      "\n"),file=outputfile,append=T)
    }
  }
}



# #################################################
# Function for preparing and printing
# the number of correct attributions
# this is a variant of the above function make.ranking.of.candidates
# Argument: table of distances
# #################################################

make.number.of.correct.attributions = function(dist.matrix) {
  corr.attrib = 0
  for(h in 1:length(dist.matrix[,1])) {
  ranked.c = order(dist.matrix[h,])
  current.sample = c(gsub("_.*","",colnames(dist.matrix)))[ranked.c]
    if((gsub("_.*","",rownames(dist.matrix)))[h] == current.sample[1]) {
    corr.attrib = corr.attrib + 1
    }
  }
# the result of the function:
return(corr.attrib)
}

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
        # link the table into the variable used for calculations
        mfw.list.of.all = features
      } else {
        cat("\n")
        cat("You seem to have chosen an existing set of features\n")
        cat("Unfortunately, something is wrong: check if your variable\n")
        cat("has a form of vector\n")
        cat("\n")
        stop("Wrong format: a vector of features was expected")
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
        features = scan(features,what="char",sep="\n")
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
        frequencies = t(read.table(frequencies))
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
     # this is stupid, but this obsolete variable is needed somewhere (?)
     mfw.list.of.all = features
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
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "Working directory should contain two subdirectories: 
        \"",training.corpus.dir,"\" and \"",test.corpus.dir,"\"\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }
  # Checking if the subdirectories contain any stuff
  if(length(filenames.primary.set) <2 | length(filenames.secondary.set) <2) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "Both subdirectories \"",training.corpus.dir,"\" and \"",
        test.corpus.dir,"\" should contain at least two text samples!\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }

  # loading text files, splitting, parsing, n-gramming, samping, and so forth
  corpus.of.primary.set = load.corpus.and.parse(files = filenames.primary.set,
                         corpus.dir = training.corpus.dir,
                         markup.type = corpus.format,
                         language = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         features = analyzed.features,
                         ngram.size = ngram.size)

  # loading text files: test set
  corpus.of.secondary.set = load.corpus.and.parse(files=filenames.secondary.set,
                         corpus.dir = test.corpus.dir,
                         markup.type = corpus.format,
                         language = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
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
    mfw.list.of.all = features
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
    mfw.list.of.all = sort(table(c(wordlist.of.primary.set,wordlist.of.secondary.set)),
                            decreasing=T)
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
      "", file="wordlist.txt", sep="\n")
    # the current wordlist into a file
    cat(mfw.list.of.all, file="wordlist.txt", sep="\n",append=F)
    
  }   # <----- conditional expr. if(features.exist == TRUE) terminates here


  # blank line on the screen
  cat("\n")


  # preparing a huge table of all the frequencies for the training set
  freq.I.set.0.culling = make.table.of.frequencies(corpus = corpus.of.primary.set,
                            words = mfw.list.of.all,
                            absent.sensitive = FALSE,
                            relative = relative.frequencies)

  # preparing a huge table of all the frequencies for the test set
  freq.II.set.0.culling = make.table.of.frequencies(corpus = corpus.of.secondary.set,
                            words = mfw.list.of.all,
                            absent.sensitive = FALSE,
                            relative = relative.frequencies)

  # writing the frequency tables to text files (they can be re-used!)
  write.table(t(freq.I.set.0.culling), 
              file="freq_table_primary_set.txt", 
              sep="\t",
              row.names=TRUE,
              col.names=TRUE)
  write.table(t(freq.II.set.0.culling), 
              file="freq_table_secondary_set.txt", 
              sep="\t",
              row.names=TRUE,
              col.names=TRUE)
  
}
###############################################################################

# #################################################
# the module for loading the corpus terminates here
# #################################################



# #################################################
# module for exporting config settings
# #################################################

# Finally, we want to save some variable values for later use
cat("",file="classify_config.txt",append=F)
var.name <- function(x) { 
      if(is.character(x)==TRUE) {
      cat(paste(deparse(substitute(x)),"=\"",x,"\"", sep=""),file="classify_config.txt",sep="\n",append=T)
        } else {
          cat(paste(deparse(substitute(x)),x, sep="="),file="classify_config.txt",sep="\n",append=T) }
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
 var.name(length.of.random.sample)
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
cat("",file=outputfile,append=F)

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
authors.I.set = c(gsub("_.*","",rownames(freq.I.set.0.culling)))
authors.II.set = c(gsub("_.*","",rownames(freq.II.set.0.culling)))
perfect.guessing = length(authors.II.set[authors.II.set %in% authors.I.set])







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

# the beginning of the culling procedure (this is to be done 
# on the primary set only; the secondary set is using the same list!)
#
raw.list.after.culling = c()

# extracting non-zero values from primary set frequency table,
# or from both frequency tables (if specified)
  if(culling.of.all.samples == FALSE) {
    nonzero.values = freq.I.set.0.culling > 0
  } else {
    nonzero.values = rbind(freq.I.set.0.culling,freq.II.set.0.culling) > 0
  }

# counting of how many non-zero values there are
for (y in 1: length(nonzero.values[1,])) {
  raw.list.after.culling = c(raw.list.after.culling, 
              (length(grep("TRUE",nonzero.values[,y])) / 
                     length(nonzero.values[,y])) 
                           >= current.culling/100 
                           )
  }
# a raw culling list has no word-identification; let's change it:
names(raw.list.after.culling) = colnames(freq.I.set.0.culling)

# a simple sequence of words which were not culled
list.of.words.after.culling = 
          c(names(raw.list.after.culling[grep("TRUE",raw.list.after.culling)]))


# procedure for deleting pronouns
if (delete.pronouns == TRUE) {
    list.of.words.after.culling = 
      list.of.words.after.culling[!(list.of.words.after.culling %in% pronouns)]
    }


# just in case: get rid of empty "words" (strings containing no characters)
list.of.words.after.culling = 
           list.of.words.after.culling[nchar(list.of.words.after.culling) >0]


# the above list-of-not-culled to be applied to both sets:
primary.set = freq.I.set.0.culling[,c(list.of.words.after.culling)]
##    rownames(primary.set) = filenames.primary.set
secondary.set = freq.II.set.0.culling[,c(list.of.words.after.culling)]
##    rownames(secondary.set) = filenames.secondary.set

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





cat("\n")
cat("culling @ ", current.culling,"\t","available words ",mfw.max,"\n")




# #################################################
# z-scores calcutations
# #################################################


# mean and standard dev. for each word (in primary set)
primary.set.mean = c(sapply(as.data.frame(primary.set), mean))
primary.set.sd = c(sapply(as.data.frame(primary.set), sd))

# calculating z-scores for both I and II sets (a message on the screen)
cat("Calculating z-scores... \n\n")


# an additional table composed of relative word frequencies 
# of joint primary and secondary sets
freq.table.both.sets = rbind(primary.set, secondary.set)



# Entropy distance: experimental, but entirely available
# (the results do not really differ than for typical word frequencies)
#
#A = t(t(freq.table.both.sets + 1) / colSums(freq.table.both.sets + 1))
#B =t(t(log(freq.table.both.sets + 2)) / -(colSums(A * log(A))))
#freq.table.both.sets = B
#



# calculating z-scores either of primary set, or of both sets
if(z.scores.of.all.samples == FALSE) {
  # function for z-scores scaling executed for primary.set
  zscores.primary.set = scale(primary.set)
  rownames(zscores.primary.set) = rownames(primary.set)
  # function for z-scores scaling executed for secondary.set
  zscores.secondary.set = 
            scale(secondary.set, center=primary.set.mean, scale=primary.set.sd)
  rownames(zscores.secondary.set) = rownames(secondary.set)
  # the two tables with calculated z-scores should be put together
  zscores.table.both.sets = rbind(zscores.primary.set, zscores.secondary.set)
} else {
  # the z-scores can be calculated on both sets as alternatively  
  zscores.table.both.sets = scale(freq.table.both.sets)
  zscores.table.both.sets = zscores.table.both.sets[,]
  }










# #################################################
# the internal loop starts here (for i = mfw.min : mfw.max)
# #################################################

# a short message on the screen:
if(distance.measure == "CD") {
  cat("Calculating classic Delta distances... \n")
  }
if(distance.measure == "AL") {
  cat("Calculating Argamon's Delta distances... \n")
  }
if(distance.measure == "ED") {
  cat("Calculating Eder's Delta distances... \n")
  }
if(distance.measure == "ES") {
  cat("Calculating Eder's Simple distances... \n")
  }
if(distance.measure == "MH") {
  cat("Calculating Mahattan distances... \n")
  }
if(distance.measure == "CB") {
  cat("Calculating Canberra distances... \n")
  }
if(distance.measure == "EU") {
  cat("Calculating Euclidean distances... \n")
  }





for(i in seq(mfw.min, mfw.max, round(mfw.incr)) ) {
mfw = i



# for safety reasons, if MFWs > words in samples
if(mfw > length(list.of.words.after.culling) ) {
  mfw = length(list.of.words.after.culling)
  }

  


# the current task (number of MFW currently analyzed) echoed on the screen
cat(mfw, " ")







# FOR SOME REASON, IT IS NEEDED AT SOME POINT, even if this is obsolete
distance.name.on.graph = "standard classification"









# Delta in its various flavours

perform.delta = function(zscores.table.both.sets, distance.measure = "CD") {
# calculating classic Delta distances
if(distance.measure == "CD") {
  distance.name.on.graph = "Classic Delta distance"
  distance.name.on.file = "Classic Delta"
  distance.table = 
            as.matrix(dist(zscores.table.both.sets[,1:mfw],
            method="manhattan")) / mfw
  }
# calculating Argamon's "Linear Delta"
if(distance.measure == "AL") {
  distance.name.on.graph = "Argamon's Delta distance"
  distance.name.on.file = "Argamon's Delta"
  distance.table = 
            as.matrix(dist(zscores.table.both.sets[,1:mfw],
            method="euclidean")) / mfw
  }
# calculating Delta distances with Eder's modifications
if(distance.measure == "ED") {
  distance.name.on.graph = "Eder's Delta distance"
  distance.name.on.file = "Eder's Delta"
  zscores.plus.e.value = t(t(zscores.table.both.sets[,1:mfw])*((1+mfw:1)/mfw))
  distance.table = as.matrix(dist(zscores.plus.e.value,method="manhattan"))
  }
# calculating Eder's Simple distance to a matrix distance.table
if(distance.measure == "ES") {
  distance.table = 
         as.matrix(dist(sqrt(freq.table.both.sets[,1:mfw]),method="manhattan"))
  distance.name.on.graph = "Eder's Simple distance"
  distance.name.on.file = "Eder's Simple"
  }
# calculating Manhattan distance to a matrix distance.table
if(distance.measure == "MH") {
  distance.name.on.graph = "Manhattan distance"
  distance.name.on.file = "Manhattan"
  distance.table = 
           as.matrix(dist(freq.table.both.sets[,1:mfw],method="manhattan"))
  }
# calculating Canberra distance to a matrix distance.table
if(distance.measure == "CB") {
  distance.name.on.graph = "Canberra distance"
  distance.name.on.file = "Canberra"
  distance.table = 
           as.matrix(dist(freq.table.both.sets[,1:mfw],method="canberra"))
  }
# calculating Euclidean distance to a matrix distance.table
if(distance.measure == "EU") {
  distance.name.on.graph = "Euclidean distance"
  distance.name.on.file = "Euclidean"
  distance.table = 
           as.matrix(dist(freq.table.both.sets[,1:mfw],method="euclid"))
  }
# replaces the names of the samples (the extension ".txt" is cut off)
rownames(distance.table)=gsub("\\.txt$","",rownames(zscores.table.both.sets))
colnames(distance.table)=gsub("\\.txt$","",rownames(zscores.table.both.sets))

# #################################################
# extracting candidates, drawing, printing, etc.

# a selected area of the distance.table is needed, with colnames()
no.of.possib = length(primary.set[,1])
no.of.candid = length(secondary.set[,1])
selected.dist = 
          as.matrix(distance.table[no.of.possib+1:no.of.candid,1:no.of.possib])

return(selected.dist)
}




perform.knn = function(training.set, test.set, k.value=1) {
  #kNN classification:
  # library(class)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  training.set = cbind(classes.training,training.set)
  test.set = cbind(classes.test,test.set)
  #
  # classes that will be used for training the classifier (=classes of I set)
  classes = factor(training.set[,1])
  # training and classification
  classification.results = knn(training.set[,-1],test.set[,-1],classes,k=k.value)
  # cross-validation: 
  #knn.cv(training.set[,-1],classes,k=k.value,prob=T)
  # get final results
  classification.results = as.character(classification.results)
return(classification.results)
}


perform.naivebayes = function(training.set, test.set) {
  # Naive Bayes classification:
  #  library(e1071)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
  input.data = as.data.frame(rbind(training.set,test.set))
  input.data = cbind(classes, input.data)
  training.classes = c(1:length(training.set[,1]))
  #
  # training a model
  model = naiveBayes(classes ~ ., data = input.data, subset = training.classes)
  #
  # testing the model on "new" data (i.e. the test.set)
  classification.results = predict(model, input.data[,-1])
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training))]
return(classification.results)
}



perform.svm = function(training.set, test.set) {
  # Support Vector Machines classification:
  # library(e1071)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
  input.data = as.data.frame(rbind(training.set,test.set))
  input.data = cbind(classes, input.data)
  training.classes = c(1:length(training.set[,1]))
  #
  # training a model
#  model = svm(classes ~ ., data = input.data, subset = training.classes)
  model = svm(classes ~ ., data = input.data, subset = training.classes, 
                 kernel = svm.kernel, degree = svm.degree, coef0 = svm.coef0, 
                 cost = svm.cost)
  #
  # testing the model on "new" data (i.e. the test.set)
  classification.results = predict(model, input.data[,-1])
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training))]
  #plot(cmdscale(dist(input.data[,-1])),col=as.integer(input.data[,1]),pch=c("o","+"))
return(classification.results)
}






perform.nsc = function(training.set, test.set) {
# Nearest Shrunken Centroid classification:
  #  library(pamr)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
  input.data = as.data.frame(rbind(training.set,test.set))
  training.classes = c(1:length(training.set[,1]))
  mydata=list(x = t(input.data),
              y = as.factor(classes),
              geneid = as.character(1:length(colnames(training.set))), 
              genenames = colnames(training.set)
              )
  # training a model
  model = pamr.train(mydata,sample.subset=c(1:length(classes.training)))
# getting the most discriminative features
#  the.features = pamr.listgenes(model,mydata,threshold=5,genenames=TRUE)[,2]
  # testing the model on "new" data (i.e. the test.set)
  classification.results = pamr.predict(model,mydata$x,threshold=1)
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training))]
return(classification.results)
}







if(tolower(classification.method) == "delta") {
  selected.dist = perform.delta(zscores.table.both.sets, distance.measure)
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
                                       test.set = secondary.set[,1:mfw])
}






classes.test = gsub("_.*","",rownames(secondary.set))



# returns the ranking of the most likely candidates as a list
if(final.ranking.of.candidates == TRUE) {
    cat("\n\n\n",file=outputfile,append=T)
    if(tolower(classification.method) == "delta") {
      make.ranking.of.candidates(selected.dist,number.of.candidates)
    } else {
      misclassified.samples = 
                   paste(rownames(secondary.set), "\t-->\t",
                   classification.results)[classes.test!=classification.results]
      cat(misclassified.samples,file=outputfile,append=T,sep="\n")    
    }
}




# returns the number of correct attributions
if(how.many.correct.attributions == TRUE) {
    if(tolower(classification.method) == "delta") {
      no.of.correct.attrib = make.number.of.correct.attributions(selected.dist)
    } else {
      no.of.correct.attrib = sum(as.numeric(classes.test == 
                                 classification.results))
    }
    total.no.of.correct.attrib = 
         c(total.no.of.correct.attrib, no.of.correct.attrib)
    total.no.of.possible.attrib = 
         c(total.no.of.possible.attrib, perfect.guessing)
    cat("\n",file=outputfile,append=T)
    cat(mfw, " MFW , culled @ ",current.culling,"%,  ",
         no.of.correct.attrib," of ", perfect.guessing,"\t(",
         round(no.of.correct.attrib / perfect.guessing * 100, 1),"%)",
         "\n",file=outputfile,append=T,sep="")
}











if(cv == "thorough") {

  cat("\n")
  cat("cross-validation...\n")
 
  # an additional table combined of frequencies of set I and II
  # just for feeding the bootstrap module 
  freq.table.both.sets.binded = rbind(primary.set[,1:mfw],secondary.set[,1:mfw])

  #bootstrap.output = "bootstrap_output.txt"
  # cleaning the bootstrapfile
  #cat("",file=bootstrap.output,append=F)

  # creating an empty matrix for the final success scores
  cross.validation.results = c()
  cross.validation.results.all = c()

  # starting two emtpy vectors
  total.no.of.correct.attrib.cv = c()
  total.no.of.possible.attrib.cv = c()
  
  

  # the beginning of k-fold cross-validation k number of iterations
  for(iterations in 1 : cv.folds) {

    names.of.training.set.orig = rownames(primary.set)
    names.of.training.set.new = c()
    classes.training.set = gsub("_.*","",names.of.training.set.orig,perl=T)
    classes.test.set = gsub("_.*","",rownames(secondary.set),perl=T)

      # this looks for classes that were not represented so far in I set
      for(i in classes.training.set) {
        # shuffle the order of samples (both sets combined)
        names.of.the.texts = sample(rownames(freq.table.both.sets.binded))
        # exclude samples that have been already chosen to the training set
        names.of.the.texts = setdiff(names.of.the.texts, names.of.training.set.new)
        # extract the classes, or strings before the first underscore
        classes.both.sets = gsub("_.*","",names.of.the.texts,perl=T)
        # find the first matching sample
        randomly.picked.sample = grep(i,classes.both.sets)[1]
        # retrieve its full name
        training.set.next.member = names.of.the.texts[randomly.picked.sample]
        # build the vector of randomly chosen members of the training set
        names.of.training.set.new = c(names.of.training.set.new, training.set.next.member)
      }

   # getting back the original samples' names
   names.of.the.texts = rownames(freq.table.both.sets.binded)


  # establishing the I set:
  training.set = freq.table.both.sets.binded[names.of.training.set.new,]

  # establishing the II set
  test.set = 
        freq.table.both.sets.binded[!(names.of.the.texts %in% names.of.training.set.new),]

  # both sets binded again, after rearrangements 
  freq.table.both.sets.binded = rbind(training.set,test.set)


  if(tolower(classification.method) == "delta") {
    current.zscores = scale(freq.table.both.sets.binded)
    selected.dist1 = perform.delta(current.zscores, distance.measure)
  }
  if(tolower(classification.method) == "knn") {
    classification.results = perform.knn(training.set,test.set)
  }
  if(tolower(classification.method) == "svm") {
    classification.results = perform.svm(training.set,test.set)
  }
  if(tolower(classification.method) == "nsc") {
    classification.results = perform.nsc(training.set,test.set)
  }


  
  # retrieving the names of classes
  classes.test = gsub("_.*","",rownames(test.set))


  
    # returns the number of correct attributions
    if(how.many.correct.attributions == TRUE) {
        if(tolower(classification.method) == "delta") {
          no.of.correct.attrib = make.number.of.correct.attributions(selected.dist1)
        } else {
          no.of.correct.attrib = sum(as.numeric(classes.test == 
                                     classification.results))
        }
      # 
      perfect.guessing.cv = 
        length(classes.training.set[classes.test.set %in% classes.training.set])
      total.no.of.correct.attrib.cv = 
                         c(total.no.of.correct.attrib.cv, no.of.correct.attrib)
      total.no.of.possible.attrib.cv = 
                         c(total.no.of.possible.attrib.cv, perfect.guessing.cv)
      cat("\n",file=outputfile,append=T)
      cat(mfw, " MFW , culled @ ",current.culling,"%,  ",
               no.of.correct.attrib," of ", perfect.guessing.cv,"\t(",
               round(no.of.correct.attrib / perfect.guessing.cv * 100, 1),"%)",
               "\n",file=outputfile,append=T,sep="")
      # percentage of correct attributions
      success.rate.cv = no.of.correct.attrib / perfect.guessing.cv * 100
      # combining results for k folds
      cross.validation.results = c(cross.validation.results, success.rate.cv)
    }

  }

  
  cross.validation.results.all = cbind(cross.validation.results.all, cross.validation.results)
  colnames(cross.validation.results.all) = paste(mfw, "@", current.culling, sep="")
}   # <-- if cv = "thorough"






if(exists("cross.validation.results.all")) {
  cross.validation.summary = cbind(cross.validation.summary, cross.validation.results.all)
  rownames(cross.validation.summary) = 1:cv.folds
}







}    # <-- the internal loop for(i) returns here
# #################################################

# blank line on the screen
cat("\n")


}    # <-- the main loop for(j) returns here
# #################################################



all.guesses = total.no.of.correct.attrib / total.no.of.possible.attrib * 100
total.no.of.correct.attrib = sum(total.no.of.correct.attrib)
total.no.of.possible.attrib = sum(total.no.of.possible.attrib)



# information about the current task into the logfile
cat("\nGeneral attributive success:  ", total.no.of.correct.attrib, " of ",
           total.no.of.possible.attrib, " (",
           round(total.no.of.correct.attrib/total.no.of.possible.attrib*100, 1), 
           "%)\n",file=outputfile,append=T,sep="")
cat("\nMFWs from ",mfw.min," to ",mfw.max.original,
                  " @ increment ",mfw.incr,"\nCulling from ",culling.min,
                  " to ",culling.max," @ increment ",culling.incr,
                  "\nPronouns deleted: ",delete.pronouns,"; ",
                  distance.name.on.graph,"\n",file=outputfile,append=T,sep="")
# additional empty line in outputfile (EOF)
cat("\n",file=outputfile,append=T)


# the same information (about the current task) on screen
cat("\nGeneral attributive success:  ", total.no.of.correct.attrib, " of ",
           total.no.of.possible.attrib, " (",
           round(total.no.of.correct.attrib/total.no.of.possible.attrib*100,1), 
           "%, sd =", round(sd(all.guesses),1),"%)\n")
cat("\nMFWs from ",mfw.min," to ",mfw.max.original,
                  " @ increment ",mfw.incr,"\nCulling from ",culling.min,
                  " to ",culling.max," @ increment ",culling.incr,
                  "\nPronouns deleted: ",delete.pronouns,"; ",
                  distance.name.on.graph,"\n",sep="")
cat("\n")


###########################################################




# Names of many variables are incredibly unfashionable: they were acceptable
# in ver. 0.0.1 of the script, which provided just a basic Delta test
# with no additional options. Since it is quite a lot of work to modernize 
# the variables' names in the code (and parhaps it is too late now...), 
# this simple wrappers will rename at least the exported variables:
success.rate = all.guesses
  if(length(success.rate) >1) {
    overall.success.rate = mean(all.guesses)
  }
frequencies.training.set = freq.I.set.0.culling
frequencies.test.set = freq.II.set.0.culling
frequencies.both.sets = freq.table.both.sets
zscores.both.sets = zscores.table.both.sets
features.actually.used = list.of.words.after.culling[start.at : mfw.max]
features = mfw.list.of.all

# what about removing some of the variables? (suppose there are thousands
# of texts and dozens of features, and only 2GB of RAM...)







# #################################################
# praparing final resutls: building a class




if(exists("misclassified.samples")) {
  attr(misclassified.samples, "description") = "............"
#  class(misclassified.samples) = "stylo.data"
}
if(exists("cross.validation.summary") & length(cross.validation.summary) >0 ) {
  attr(cross.validation.summary, "description") = "correctly guessed samples (cross-validation folds)"
  class(cross.validation.summary) = "stylo.data"
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
if(exists("nsc.distinctive.features")) {
  attr(nsc.distinctive.features, "description") = "most discriminative features for NSC procedure"
# this sucks
#  class(nsc.distinctive.features) = "stylo.data"
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


# creating an object (list) that will contain the final results,
# tables of frequencies, etc.etc.
results.classify = list()
# elements that we want to add on this list
variables.to.save = c("misclassified.samples",
                      "success.rate",
                      "overall.success.rate",
                      "distance.table", 
                      "nsc.distinctive.features",
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
class(results.classify) <- "stylo.results"






# back to the original working directory
setwd(original.path)

# return the value of the function 
return(results.classify)
}


