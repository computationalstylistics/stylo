

# there is still a lot of work with classify():
# (1) gui should be extracted as a separated function
# (2) it will be better to have initial settings stored in a separate function,
#     provided that this is a good idea (cf. stylo() and stylo.default.settings())



classify <-
function(gui = TRUE, path = "",
         training.corpus.dir = "primary_set",
         test.corpus.dir = "secondary_set") {

# so far, 'training.corpus' and 'test.corpus' are not used
# (certainly: they are used, but their values are specified elsewhere)




# changing working directory (if applicable)
#
# first of all, retrieve the current path name
original.path = getwd()
# then check if anywone wants to change the working dir
if(is.character(path) == TRUE & nchar(path) > 0) {
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




# most settings will be loaded using stylo.default.settings()
# some classify-specific options, however, have to be declared here:



#######  MATHEMATICAL SETTINGS (CLASSIFICATION METHOD)  #############

# method of classification: choose one of the options described below
# Delta ("delta"), k-nearest neighbor classification ("knn"),
# Naive Bayes classification ("naivebayes"), Nearest Shrunken Centroids
# ("nsc"), or Support Vectors Machines ("svm")
classification.method <<- "delta"

# Delta is always active: output is directed to a file. You may specify
# the number of final ranking candidates to be displayded (at least 1)
number.of.candidates <<- 3

# Report the number of correct guesses for each iteration (written to 
# the log file). Ranking of the least unlikely candidates in the log file.
how.many.correct.attributions <<- TRUE
final.ranking.of.candidates <<- TRUE

# How the z-scores should be calculated:
# if the variable is set to FALSE, then the z-scores are relying
# on the primary set only (this should be better in most cases; after all,
# this is the classical solution used by Burrows and Hoover).
# Otherwise, the scaling is based on all the values
# in the primary and the secondary sets.
z.scores.of.all.samples <<- FALSE

# The both talbes of frequencies are build using the pre-prepared word
# list of the whole I set. Alternatively, one might want to prepare 
# this list of both sets. Similarily culling: it can be calcutated either 
# on the I set, or on both sets
reference.wordlist.of.all.samples <<- FALSE
culling.of.all.samples <<- TRUE

# file with the final ranking of Delta results (log file)
outputfile <<- "final_results.txt"

# THIS WILL BE OBSOLETE ONCE make.parallel.freq.list is replaced
# (carefully: it is used by GUI as well...)
random.sampling <<- FALSE






# loading the default settings as defined in the following function
stylo.default.settings()





# #################################################
# checking some of the initial variables -- just in case
# #################################################

# This prevents us from choosing a non-existing distance measure -- in such
# case the default distance (Classic Delta) will be switched on. Be aware
# of correct spelling: then the default value will be assigned as well!

if(distance.measure %in% c("CD","AL","ED","ES","MH","CB","EU") == FALSE) {
  distance.measure = "CD"
  }

classification.method = tolower(classification.method)


# #################################################


# this is obsolete, but still required somewhere below (bug to be fixed)
consensus.strength = 0.5





# optionally, displaying a GUI box
if (gui == TRUE) {
  gui.classify()
  } 





# #############################################################################
# Final settings (you are advised rather not to change them)
# #############################################################################


# Given a language option ("English", "Polish", "Latin" etc., as described 
# above), this procedure selects one of the lists of pronouns
# If no language was chosen (or if a desired language is not supported, or if 
# there was a spelling mistake), then the variable will be set to "English". 

pronouns = stylo.pronouns(language=corpus.lang)

# A chosen language option should be followed by an assignment of 
# the appropriate set of pronouns. The following code is responsible for it
  if(corpus.lang == "English")
      pronouns = eng.pronouns
  if(corpus.lang == "Polish")
      pronouns = pol.pronouns 
  if(corpus.lang == "Latin")
      pronouns = lat.pronouns
  if(corpus.lang == "French")
      pronouns = fra.pronouns
  if(corpus.lang == "German" )
      pronouns = ger.pronouns
  if(corpus.lang == "Italian")
      pronouns = ita.pronouns
  if(corpus.lang == "Hungarian")
      pronouns = hun.pronouns


  # Windows users are a bit allergic to Unicode; let's make them happy
  # by converting the chosen set of pronouns to local encoding
  if(Sys.info()[["sysname"]] == "Windows") { 
    pronouns = iconv(pronouns, from="UTF-8")
  }


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
 var.name(random.sampling)
 var.name(length.of.random.sample)
 var.name(classification.method)
 var.name(mfw.min)
 var.name(mfw.max)
 var.name(mfw.incr)
 var.name(start.at)
 var.name(mfw.list.cutoff)
 var.name(culling.min)
 var.name(culling.max)
 var.name(culling.incr)
 var.name(delete.pronouns)
 var.name(culling.of.all.samples)
 var.name(final.ranking.of.candidates)
 var.name(how.many.correct.attributions)
 var.name(use.existing.freq.tables)
 var.name(use.existing.wordlist)
 var.name(distance.measure)
 var.name(number.of.candidates)
 var.name(z.scores.of.all.samples)
 var.name(reference.wordlist.of.all.samples)


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
#
#
# Checking: (1) whether produce new frequency tables or use existing ones;
# (2) whether the tables are stored in memory or written into files.
# If you have chosen using the existing tables and there are no such tables
# available, then your choice will be ignored
if(use.existing.freq.tables == TRUE 
            && file.exists("freq_table_primary_set.txt") == TRUE
            && file.exists("freq_table_secondary_set.txt") == TRUE
            ) { 
      if(exists("freq.I.set.0.culling") && exists("freq.II.set.0.culling")) {
      cat("\n", "using frequency tables stored as variables...", "\n")
        } else {
          cat("\n", "reading files with frequency tables...", "\n")
          freq.I.set.0.culling = t(read.table("freq_table_primary_set.txt"))
          freq.II.set.0.culling = t(read.table("freq_table_secondary_set.txt"))
          cat("\n", "frequency tables loaded successfully", "\n\n")
        }
      # extracting names of the samples
      filenames.primary.set = rownames(freq.I.set.0.culling)
      filenames.secondary.set = rownames(freq.II.set.0.culling)
      #
      # checking whether existing wordlist should be used as well
      if (use.existing.wordlist == TRUE && file.exists("wordlist.txt") == TRUE){
          cat("\n", "reading a wordlist from file...", "\n")
          mfw.list.of.all = scan("wordlist.txt",what="char",sep="\n")
          mfw.list.of.all = c(grep("^[^#]",mfw.list.of.all,value=TRUE))
          #
          # adjusting the size of frequency tables with the existing wordlist
          freq.I.set.0.culling = 
                       freq.I.set.0.culling[,colnames(freq.I.set.0.culling) 
                       %in% mfw.list.of.all]
          freq.II.set.0.culling = 
                       freq.II.set.0.culling[,colnames(freq.II.set.0.culling) 
                       %in% mfw.list.of.all]
      } else {
          # the wordlist will be created from existing tables with frequencies
          mfw.list.of.all = colnames(freq.I.set.0.culling)
          # some comments into the file containing wordlist
          cat("# This file contains the words that were used in the table",
          "# of frequencies uploaded from an external file. The current list",
          "# can be used for the next tasks, and for this purpose it can be",
          "# manually revised, edited, deleted, culled, etc.", 
          "# You can either delete unwanted words, or mark them with \"#\"",
          "# -----------------------------------------------------------------",
          "",
          file="wordlist.txt", sep="\n")
          # the current wordlist into a file
          cat(mfw.list.of.all, file="wordlist.txt", sep="\n",append=F)
        }
# if existing tables will not be used, then begin producing new tables
  } else {
#
# Retrieving the names of samples
#
filenames.primary.set = list.files("primary_set")
filenames.secondary.set = list.files("secondary_set")
#
# Checking whether required files and subdirectories exist
if(file.exists("primary_set")==FALSE || file.exists("secondary_set")==FALSE) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Working directory should contain two subdirectories: 
    \"primary_set\" and \"secondary_set\"\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    stop("corpus prepared incorrectly")
    }
if(length(filenames.primary.set) < 2 || length(filenames.secondary.set) < 2) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Both subdirectories \"primary_set\" and \"secondary_set\"
    should contain at least two text samples!\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    stop("corpus prepared incorrectly")
    }
#
#
#
#################################################################
#################################################################

corpus.of.primary.set = load.corpus.and.parse(files=filenames.primary.set,
                         corpus.dir=training.corpus.dir,
                         markup.type=corpus.format,
                         language=corpus.lang,
                         sample.size=sample.size,
                         sampling=sampling,
                         sampling.with.replacement=sampling.with.replacement,
                         features=analyzed.features,
                         ngram.size=ngram.size)

corpus.of.secondary.set = load.corpus.and.parse(files=filenames.secondary.set,
                         corpus.dir=test.corpus.dir,
                         markup.type=corpus.format,
                         language=corpus.lang,
                         sample.size=sample.size,
                         sampling=sampling,
                         sampling.with.replacement=sampling.with.replacement,
                         features=analyzed.features,
                         ngram.size=ngram.size)


#################################################################
#################################################################

# blank line on the screen
cat("\n")
#
#
#
# both directories (primary_set and secondary_set) shoud contain some texts;
# if the number of text samples is lower than 2, the script will stop
if(length(corpus.of.primary.set) < 2 || length(corpus.of.secondary.set) < 2) {
    cat("\n\n","either the training set or the test set is empty!", "\n\n")
    stop("corpus error")
    }
#
#
# We need a list of the most frequent words used in the current corpus, 
# in descending order, without frequencies (just a list of words). It can be 
# either loaded from a file (then set the option "use.existing.wordlist=TRUE"), 
# or created by the code provided below:
#
if (use.existing.wordlist == TRUE && file.exists("wordlist.txt") == TRUE) {
          cat("\n", "reading a wordlist from file...", "\n")
          mfw.list.of.all = scan("wordlist.txt",what="char",sep="\n")
          mfw.list.of.all = c(grep("^[^#]",mfw.list.of.all,value=TRUE))
} else {
# Extracting all the words used in the texts of primary set 
# (or both if "Z-scores all" is set to TRUE)
#
wordlist.of.primary.set = c()
  cat("\n")
  for (file in 1 : length(corpus.of.primary.set)) {
    # loading the next sample from the list filenames.primary.set,
    current.text = corpus.of.primary.set[[file]]
    # putting samples together:
    wordlist.of.primary.set = c(wordlist.of.primary.set, current.text)
    cat(".")
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
    cat(".")
    }
  } else {
  wordlist.of.secondary.set = c()}
#
#
# preparing a sorted frequency list of the whole primary set
mfw.list.of.all = sort(table(
                  c(wordlist.of.primary.set,wordlist.of.secondary.set)),
                  decreasing=T)
  # if the whole list is long, then cut off the tail (e.g., > 5000 mfw)
  if (length(mfw.list.of.all) > mfw.list.cutoff) {
  mfw.list.of.all = mfw.list.of.all[1:mfw.list.cutoff]
  }
# the only thing we need are words ordered by frequency (no frequencies)
mfw.list.of.all = names(mfw.list.of.all)
#
# some comments into the file containing wordlist
cat("# This file contains the words that were used for building the table",
  "# of frequencies. It can be also used for the next tasks, and for this",
  "# purpose it can be manually revised, edited, deleted, culled, etc.", 
  "# You can either delete unwanted words, or mark them with \"#\"",
  "# -----------------------------------------------------------------------",
  "",
      file="wordlist.txt", sep="\n")
# the current wordlist into a file
cat(mfw.list.of.all, file="wordlist.txt", sep="\n",append=F)
#
}   # <----- conditional expr. "use.existing.wordlist" terminates here
#
# 
#
#
# blank line on the screen
cat("\n")
#
#



#################################################################
#################################################################

# preparing a huge table of all the frequencies for the training set
freq.I.set.0.culling = make.table.of.frequencies(corpus = corpus.of.primary.set,
                            words = mfw.list.of.all,
                            absent.sensitive=FALSE)

# preparing a huge table of all the frequencies for the test set
freq.II.set.0.culling = make.table.of.frequencies(corpus = corpus.of.secondary.set,
                            words = mfw.list.of.all,
                            absent.sensitive=FALSE)

#################################################################
#################################################################




#
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
#
}  # <----- conditional expr. "use.existing.freq.tables" terminates here
#
#
# #################################################
# the module for loading the corpus terminates here
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

# load the ape library; make an empty bootstrap.results list
# this will be executed only if the bootstrap option was checked
#if (make.consensus.tree == TRUE) {
#    library(ape)
#    bootstrap.list = list()
#    }






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


# the above list-of-not-culled to be applied to both sets:
primary.set = freq.I.set.0.culling[,c(list.of.words.after.culling)]
    rownames(primary.set) = filenames.primary.set
secondary.set = freq.II.set.0.culling[,c(list.of.words.after.culling)]
    rownames(secondary.set) = filenames.secondary.set

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
# if too small, it is set to 1 (i.e., minimal value)
  if(mfw.min < 1) {
  mfw.min = 1
  }
# if culling is too strong, sometimes strange things may happen; let's block it
  if(mfw.min > mfw.max) {
  mfw.min = mfw.max
  }
# MFW set to mfw.max for a while (it will change later on)
mfw = mfw.max



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
freq.table.both.sets = rbind(primary.set[,1:mfw.max], secondary.set[,1:mfw.max])



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



for(i in (mfw.min/mfw.incr):(mfw.max/mfw.incr)) {
mfw = i * mfw.incr

# for safety reasons, if MFWs > words in samples
if(mfw > length(list.of.words.after.culling) ) {
  mfw = length(list.of.words.after.culling)
  }

# the general counter for different purposes
number.of.current.iteration = number.of.current.iteration + 1



# the current task (number of MFW currently analyzed) echoed on the screen
cat(mfw, " ")









# #################################################
# module for Delta
# #################################################

if(tolower(classification.method) == "delta") {

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

}  # <--- delta


# the code for SVM, kNN, NSC, NaiveBayes
#
#
#
#
#
#

# FOR SOME REASON, IT IS NEEDED AT SOME POINT, even if this is obsolete
distance.name.on.graph = "to be deleted"





if(tolower(classification.method) == "knn") {
  #kNN classification:
  library(class)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(zscores.primary.set))
  training.set = cbind(classes.training,zscores.primary.set[,1:mfw])
  classes.test = gsub("_.*","",rownames(zscores.secondary.set))
  test.set = cbind(classes.test,zscores.secondary.set[,1:mfw])
#  input.data = as.data.frame(rbind(training.set,test.set))
  #
  # number of nearest neighbors to be considered
  k.value = 1
  # classes that will be used for training the classifier (=classes of I set)
  classes = factor(training.set[,1])
  # training and classification
  classification.results = knn(training.set[,-1],test.set[,-1],classes,k=k.value)
  # cross-validation: 
  #knn.cv(training.set[,-1],classes,k=k.value,prob=T)
  classification.results = as.character(classification.results)
}


if(tolower(classification.method) == "naivebayes") {
  # Naive Bayes classification:
  library(e1071)
  #
  # training_set and test_set preparation; adding class labels to both sets
  training.set = primary.set[,1:mfw]
  test.set = secondary.set[,1:mfw]
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
}



if(tolower(classification.method) == "svm") {
  # Support Vector Machines classification:
  library(e1071)
  #
  # training_set and test_set preparation; adding class labels to both sets
  training.set = primary.set[,1:mfw]
  test.set = secondary.set[,1:mfw]
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
  input.data = as.data.frame(rbind(training.set,test.set))
  input.data = cbind(classes, input.data)
  training.classes = c(1:length(training.set[,1]))
  #
  # training a model
  model = svm(classes ~ ., data = input.data, subset = training.classes)
  #
  # testing the model on "new" data (i.e. the test.set)
  classification.results = predict(model, input.data[,-1])
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training))]
#plot(cmdscale(dist(input.data[,-1])),col=as.integer(input.data[,1]),pch=c("o","+"))
}



if(tolower(classification.method) == "nsc") {
  # Nearest Shrunken Centroid classification:
  library(pamr)
  #
  # training_set and test_set preparation; adding class labels to both sets
  training.set = primary.set[,1:mfw]
  test.set = secondary.set[,1:mfw]
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
  input.data = as.data.frame(rbind(training.set,test.set))
  training.classes = c(1:length(training.set[,1]))
  mydata=list(x=t(input.data),y=as.factor(classes))
  # training a model
  model = pamr.train(mydata,sample.subset=c(1:length(classes.training)))
  # testing the model on "new" data (i.e. the test.set)
  classification.results = pamr.predict(model,mydata$x,threshold=1)
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training))]
}












# returns the ranking of the most likely candidates as a list
if(final.ranking.of.candidates == TRUE) {
    cat("\n\n\n",file=outputfile,append=T)
    if(tolower(classification.method) == "delta") {
      make.ranking.of.candidates(selected.dist,number.of.candidates)
    } else {
      misclassified.samples = 
                   paste(rownames(test.set), "\t-->\t",
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





# #################################################
# final cleaning



cat("\n")
cat("removing most of the variables... \n")
cat("type ls() if you want to see what was not removed\n")
cat("if you are going to change the corpus, clean all: rm(list=ls())\n")
cat("\n")
cat("Results saved in", outputfile, "\n")
cat("\n")
cat("for suggestions how to cite this software, type: citation(\"stylo\")\n")


# a list of variables not to be removed
do.not.remove = c("zscores.table.both.sets", "freq.table.both.sets",
                  "freq.I.set.0.culling", "freq.II.set.0.culling",
                  "distance.table","outputfile","all.guesses")

# removing the variables which are not on the above list
list.of.variables = ls()
rm(list=list.of.variables[!(list.of.variables %in% do.not.remove)])

}


