
# Function that performs a number of machine-learning methods
# of classification used in computational stylistics: Delta (Burrows, 2002), 
# k-Nearest Neighbors classification, Support Vectors Machines, Naive Bayes, 
# and Nearest Shrunken Centroids (Jockers and Witten, 2010). Most of the options 
# are derived from the 'stylo' function.

classify <-
function(gui = TRUE, path = "",
         training.corpus.dir = "primary_set",
         test.corpus.dir = "secondary_set", ...) {



# if any command-line arguments have been passed by a user, they will
# be stored on the following list and used to overwrite the defaults
passed.arguments = list(...)


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
filenames.primary.set = list.files(training.corpus.dir)
filenames.secondary.set = list.files(test.corpus.dir)
#
# Checking whether required files and subdirectories exist
if(file.exists(training.corpus.dir)==FALSE || file.exists(test.corpus.dir)==FALSE) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Working directory should contain two subdirectories: 
    \"",training.corpus.dir,"\" and \"",test.corpus.dir,"\"\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    stop("corpus prepared incorrectly")
    }
if(length(filenames.primary.set) < 2 || length(filenames.secondary.set) < 2) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Both subdirectories \"",training.corpus.dir,"\" and \"",
    test.corpus.dir,"\" should contain at least two text samples!\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
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
if(length(corpus.of.primary.set) < 2 || length(corpus.of.secondary.set) < 1) {
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
####freq.table.both.sets = rbind(primary.set[,1:mfw.max], secondary.set[,1:mfw.max])
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





for(i in seq(mfw.min,mfw.max,round(mfw.incr)) ) {
mfw = i



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
#  library(class)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(zscores.primary.set))
  training.set = cbind(classes.training,zscores.primary.set[,1:mfw])
  classes.test = gsub("_.*","",rownames(zscores.secondary.set))
  test.set = cbind(classes.test,zscores.secondary.set[,1:mfw])
#  input.data = as.data.frame(rbind(training.set,test.set))
  #
  # classes that will be used for training the classifier (=classes of I set)
  classes = factor(training.set[,1])
  # training and classification ('k' and 'l' are set in 'stylo.default.settings')
  classification.results = knn(training.set[,-1],test.set[,-1],
                              classes, k = k.value, l = l.value)
  # cross-validation: 
  #knn.cv(training.set[,-1],classes,k=k.value,prob=T)
  classification.results = as.character(classification.results)
}


if(tolower(classification.method) == "naivebayes") {
  # Naive Bayes classification:
#  library(e1071)
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
#  library(e1071)
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
  model = svm(classes ~ ., data = input.data, subset = training.classes,
              kernel = svm.kernel, degree = svm.degree,
              coef0 = svm.coef0, cost = svm.cost)
  #
  # testing the model on "new" data (i.e. the test.set)
  classification.results = predict(model, input.data[,-1])
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training))]
#plot(cmdscale(dist(input.data[,-1])),col=as.integer(input.data[,1]),pch=c("o","+"))
}



if(tolower(classification.method) == "nsc") {
  # Nearest Shrunken Centroid classification:
#  library(pamr)
  #
  # training_set and test_set preparation; adding class labels to both sets
  training.set = primary.set[,1:mfw]
  test.set = secondary.set[,1:mfw]
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
  input.data = as.data.frame(rbind(training.set,test.set))
  training.classes = c(1:length(training.set[,1]))
  mydata=list(x=t(input.data),y=as.factor(classes),geneid=as.character(1:length(colnames(training.set))), genenames=colnames(training.set))
  # training a model
  model = pamr.train(mydata,sample.subset=c(1:length(classes.training)))

  nsc.distinctive.features = pamr.listgenes(model,mydata,threshold=5,genenames=TRUE)[,2]

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







# creating an object (list) that will contain the final results,
# tables of frequencies, etc.etc.
results.classify = list()
# elements that we want to add on this list
variables.to.save = c("misclassified.samples",
                  "all.guesses", "distance.table", "nsc.distinctive.features",
                  "freq.table.both.sets", "zscores.table.both.sets",
                  "freq.I.set.0.culling", "freq.II.set.0.culling")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.classify[[i]] = get(i)
}




# #################################################
# final cleaning


cat("\n")
cat("Some results should have been written into a few files; you should\n")
cat("be able to find them in your current (working) directory. These include\n")
cat("a list of words used to build a table of frequencies, the table itself,\n")
cat("a file containg recent configuration, classification results, etc.\n")
cat("Advanced users: you can pipe the results to a variable, e.g.:\n")
cat("    my.results = classify()\n")
cat("this will create a list containing some presumably interesting stuff.\n")
cat("The list created, you can type, e.g.:\n")
cat("    summary(my.results)\n")
cat("to see which variables are stored there.\n")
cat("\n")
cat("\n")
cat("for suggestions how to cite this software, type: citation(\"stylo\")\n")
cat("\n")
cat("\n")




# back to the original working directory
setwd(original.path)

# return (tacitly) the value of the function 
invisible(results.classify)
}


