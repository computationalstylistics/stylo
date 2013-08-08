
# #################################################
# Function for applying a variety of multidimensional
# explanatory analyses to a collection of (literary) texts.
# To use this function, one needs to have a directory
# (it's name is customizable) containing some texts.
# Optional arguments: (1) GUI mode (default: TRUE),
# (2) path to your working directory, (3) name of
# the subdirectory containing the texts (default: "corpus")
# #################################################

stylo <-
function(gui = TRUE, path = "", corpus.dir = "corpus", ...) {



# if any command-line arguments have been passed by a user, they will
# be stored on the following list and used to overwrite the defaults
passed.arguments = list(...)



# this is needed at some point; in next versions, it should be 
# removed from here
all.connections = 0




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

if(is.character(corpus.dir) == FALSE | nchar(corpus.dir) == 0) {
corpus.dir = "corpus"
}



# loading the default settings as defined in the following function
# (it absorbes the arguments passed from command-line)
variables = stylo.default.settings(...)



# optionally, displaying a GUI box
# (it absorbes the arguments passed from command-line)
if (gui == TRUE) {
  variables = gui.stylo(...)
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

# If no language was chosen (or if a desired language is not supported, or if 
# there was a spelling mistake), then the variable will be set to "English". 

pronouns = stylo.pronouns(language=corpus.lang)


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
     cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Oops! To use TXM compatibility mode, you have to launch TXM first!\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    stop("Incorrect input data")
  }
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



 


# #################################################
# the module for loading a corpus from the text files;
# it can be omitted if the frequency table already exists
# (then "use.existing.freq.tables" should be set 
# to TRUE in the preamble of the script/GUI)
# #################################################
#
# Checking: (1) whether to produce a new frequency table or to use 
# the existing one; (2) whether the tables are stored in memory or 
# written into files.
# If you have chosen using the existing table and it does not exist,
# then your choice will be ignored and the table will be 
# created from scratch.


# checking if an appropriate frequency table exists
if(exists("frequencies.0.culling") == FALSE 
              && file.exists("table_with_frequencies.txt") == FALSE ) {
  use.existing.freq.tables = FALSE
}


if(use.existing.freq.tables == TRUE) { 
      if(exists("frequencies.0.culling")) {
      cat("\n", "using frequency table stored as variables...", "\n")
        } else {
          cat("\n", "reading file with frequency table...", "\n")
          frequencies.0.culling = t(read.table("table_with_frequencies.txt"))
          cat("\n", "frequency table loaded successfully", "\n\n")
        }
      # extracting names of the texts
      corpus.filenames = rownames(frequencies.0.culling)
      #
      # checking whether an existing wordlist should be used
      if (use.existing.wordlist == TRUE && file.exists("wordlist.txt") == TRUE){
          cat("\n", "reading a wordlist from file...", "\n")
          mfw.list.of.all = scan("wordlist.txt",what="char",sep="\n")
          mfw.list.of.all = c(grep("^[^#]",mfw.list.of.all,value=TRUE))
          #
          # adjusting the size of the frequency table according to the existing wordlist
          frequencies.0.culling = 
                       frequencies.0.culling[,colnames(frequencies.0.culling) 
                       %in% mfw.list.of.all]
      } else {
          # the wordlist will be created from the existing frequency tables
          mfw.list.of.all = colnames(frequencies.0.culling)
          # some comments into the file containing the wordlist
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
# if the existing table will not be used, then begin producing the new table
  } else {
#
# Retrieving the names of texts
#
# first, it's possible to choose the files manually
if (interactive.files == TRUE) {
    setwd(corpus.dir)
  corpus.filenames = basename(tk_choose.files(default = "", caption = "Select at least 2 files", multi = TRUE))
  setwd("..")
} else {
  # alternatively, one can use the files listed in "files_to_analyze.txt";
  # the listed files can be separated by spaces, tabs, or newlines
  if(use.custom.list.of.files == TRUE 
      && file.exists("files_to_analyze.txt") == TRUE) { 
    # a message on the screen
    cat("\n")
    cat("external list of files will be used for uploading the corpus\n\n")
    # retrieving the filenames from a file
    corpus.filenames = scan("files_to_analyze.txt",what="char",sep="\n",quiet=T)
    # getting rid of spaces and/or tabs
    corpus.filenames = unlist(strsplit(corpus.filenames,"[ \t]+"))
      # checking whether all the files indicated on the list really exist
      if( length(setdiff(corpus.filenames,list.files(corpus.dir))) > 0 ){
        # if not, then sent a message and list the suspicious filenames
        cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
        cat("the following files have not been found:\n")
        cat(setdiff(corpus.filenames, list.files(corpus.dir)),"\n\n")
        cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
        # use only those files that match
        corpus.filenames = intersect(corpus.filenames, list.files(corpus.dir))
      }
  } else {
  corpus.filenames = list.files(corpus.dir)
  }
}
#
# Checking whether the required files and subdirectory exist
if(file.exists(corpus.dir) == FALSE) {
    cat("\n\n", "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Hey! The working directory should contain the subdirectory \"",
    corpus.dir,"\"\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    stop("Corpus prepared incorrectly")
    }
if(length(corpus.filenames) < 2 && sampling != "normal.sampling")  {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
    "Ho! The subdirectory \"",corpus.dir,"\" should contain at least 
    two text samples!\n",
    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    stop("Corpus prepared incorrectly")
    }
#
# loading the corpus from individual text files
loaded.corpus = list()
if (sampling == "normal.sampling"){
  cat(paste("Performing sampling (using sample size = ", sample.size," words)\n", sep=""))
} else if(sampling == "random.sampling"){
  cat(paste("Performing random sampling (using random sample size = ", " words)\n", sep=""))
} else if (sampling == "no.sampling"){
cat(paste("Performing no sampling (using entire text as sample)", "\n", sep=""))
} else {
  stop("Exception raised: something is wrong with the sampling parameter you have specified...")
}



#################################################################
#################################################################

loaded.corpus = load.corpus.and.parse(files=corpus.filenames,
                         corpus.dir=corpus.dir,
                         markup.type=corpus.format,
                         language=corpus.lang,
                         sample.size=sample.size,
                         sampling=sampling,
                         sampling.with.replacement=sampling.with.replacement,
                         features=analyzed.features,
                         ngram.size=ngram.size)

#################################################################
#################################################################



cat(paste("Total nr. of samples in the corpus: ", length(loaded.corpus), "\n"))


# the directory with corpus must contain enough texts;
# if the number of text samples is lower than 2, the script will abort.
if( (length(corpus.filenames) < 2) & (sampling == "no.sampling") ) {
    cat("\n\n","your corpus folder seems to be empty!", "\n\n")
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
          # loading the wordlist fil  e, changing to lowercase
          mfw.list.of.all = tolower(scan("wordlist.txt",what="char",sep="\n"))
          # getting rid of commented lines in the wordlist file
          mfw.list.of.all = c(grep("^[^#]",mfw.list.of.all,value=TRUE))
} else {
# Extracting all the words used in the corpus
#
wordlist.of.loaded.corpus = c()
  for (file in 1 : length(loaded.corpus)) {
    # loading the next sample from the list "corpus.filenames"
    current.text = loaded.corpus[[file]]
    # putting samples together:
    wordlist.of.loaded.corpus = c(wordlist.of.loaded.corpus, current.text)
#    cat(names(loaded.corpus[file]),"\t","tokenized successfully", "\n")
    }
#
# preparing a sorted frequency list of the whole set
mfw.list.of.all = sort(table(c(wordlist.of.loaded.corpus)),decreasing=T)
  # if the whole list is long, then cut off the tail, as specified in the GUI 
  # by the cutoff value
  if (length(mfw.list.of.all) > mfw.list.cutoff) {
    mfw.list.of.all = mfw.list.of.all[1:mfw.list.cutoff]
  }
# the only thing we need are words ordered by frequency (no frequencies)
mfw.list.of.all = names(mfw.list.of.all)
#
# some comments into the file containing the wordlist
cat("# This file contains the words that were used for building the table",
  "# of frequencies. It can be also used for further tasks, and for this",
  "# purpose it can be manually revised, edited, deleted, culled, etc.", 
  "# You can either delete unwanted words, or mark them with \"#\"",
  "# -----------------------------------------------------------------------",
  "",
      file="wordlist.txt", sep="\n")
# the current wordlist into the "wordlist.txt" file
cat(mfw.list.of.all, file="wordlist.txt", sep="\n",append=T)
#
}   # <----- conditional expr. "use.existing.wordlist" terminates here
#
# blank line on the screen
cat("\n")
	  
# empty the dump-dir if it already existed and create it if it did not previously exist:
if(dump.samples == TRUE){
	if (file.exists("sample_dump")){
		# a dump-dir seems to have been created during a previous run
		# tmp delete the dump-dir to remove all of its previous contents
		unlink("sample_dump", recursive=TRUE) 
	}
	# (re)create the dump-dir
	dir.create("sample_dump")
}




#################################################################
#################################################################

# preparing a huge table of all the frequencies for the whole corpus
frequencies.0.culling = make.table.of.frequencies(corpus = loaded.corpus,
                            words = mfw.list.of.all)


#################################################################
#################################################################


#
#
#
# writing the table with frequencies to a text file (it can be re-used!)
write.table(t(frequencies.0.culling), 
            file="table_with_frequencies.txt", 
            sep="\t",
            row.names=TRUE,
            col.names=TRUE)
}  # <----- conditional expr. "use.existing.freq.tables" terminates here
#
#
# #################################################
# the module for loading the corpus terminates here
# #################################################




# #################################################
# module for saving current config options
# #################################################

# Finally, we want to save some of the variable values for later use;
# they are automatically loaded into the GUI at the next run of the script.
cat("",file="stylo_config.txt",append=F)
var.name<-function(x) { 
      if(is.character(x)==TRUE) {
      cat(paste(deparse(substitute(x))," = \"",x,"\"", sep=""),file="stylo_config.txt",sep="\n",append=T)
        } else {
          cat(paste(deparse(substitute(x)),x, sep=" = "),file="stylo_config.txt",sep="\n",append=T) }
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
var.name(use.custom.list.of.files)
var.name(analysis.type)
var.name(consensus.strength)
var.name(distance.measure)
var.name(sampling)
var.name(sample.size)
var.name(length.of.random.sample)
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

# the beginning of the culling procedure 
raw.list.after.culling = c()

# extracting non-zero values the frequency table.
nonzero.values = frequencies.0.culling > 0


# counting non-zero values
for (y in 1: length(nonzero.values[1,])) {
  raw.list.after.culling = c(raw.list.after.culling, 
              (length(grep("TRUE",nonzero.values[,y])) / 
                     length(nonzero.values[,y])) 
                           >= current.culling/100 
                           )
}
# a raw culling list has no word-identification; let's change it:
names(raw.list.after.culling) = colnames(frequencies.0.culling)
# a simple sequence of words which have not been culled

list.of.words.after.culling = c(names(raw.list.after.culling[grep("TRUE",raw.list.after.culling)]))

# procedure for deleting pronouns
if (delete.pronouns == TRUE) {
    list.of.words.after.culling = 
      list.of.words.after.culling[!(list.of.words.after.culling %in% pronouns)]
}

# the above list-of-not-culled to be applied to the wordlist:
table.with.all.freqs = frequencies.0.culling[,c(list.of.words.after.culling)]

# the names of the samples are passed to the frequency table
if(use.existing.freq.tables == FALSE) {
  rownames(table.with.all.freqs) = names(loaded.corpus)
}
  
# #################################################
# culling is done, but we are still inside the main loop

# starting the frequency list at frequency rank set in option start.at above
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



cat("\n\n")
cat("culling @ ", current.culling,"\t","available words ",mfw.max,"\n")


# #################################################
# z-scores calcutations
# #################################################

if((analysis.type == "CA") || (analysis.type == "BCT") || (analysis.type == "MDS")){
  # calculating z-scores (a message on the screen)
  cat("Calculating z-scores... \n\n")
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
    cat("Calculating Manhattan distances... \n")
  }
  if(distance.measure == "CB") {
    cat("Calculating Canberra distances... \n")
  }
  if(distance.measure == "EU") {
    cat("Calculating Euclidean distances... \n")
  }
}









cat("MFW used: ")

for(i in seq(mfw.min,mfw.max,round(mfw.incr)) ) {
mfw = i


# for safety reasons, if MFWs > words in samples
if(mfw > length(list.of.words.after.culling) ) {
  mfw = length(list.of.words.after.culling)
}

# the general counter for various purposes
number.of.current.iteration = number.of.current.iteration + 1

# the current task (number of MFW currently analyzed) echoed on the screen
cat(mfw, " ")

# #################################################
# module for calculating distances between texts
# #################################################

if((analysis.type == "CA") || (analysis.type == "BCT") || (analysis.type == "MDS")){
  # calculating Delta distances to a distance matrix
  if(distance.measure == "CD") {
    distance.name.on.graph = "Classic Delta distance"
    distance.name.on.file = "Classic Delta"
    distance.table = 
        as.matrix(dist(table.with.all.zscores[,1:mfw],
        method="manhattan")) / mfw
    }

  # calculating Argamon's "Linear Delta"
  if(distance.measure == "AL") {
    distance.name.on.graph = "Argamon's Delta distance"
    distance.name.on.file = "Argamon's Delta"
    distance.table = 
        as.matrix(dist(table.with.all.zscores[,1:mfw],
        method="euclidean")) / mfw
    }

  # calculating Delta distances with Eder's modifications
  if(distance.measure == "ED") {
    distance.name.on.graph = "Eder's Delta distance"
    distance.name.on.file = "Eder's Delta"
    zscores.plus.e.value = t(t(table.with.all.zscores[,1:mfw])*((1+mfw:1)/mfw))
    distance.table = as.matrix(dist(zscores.plus.e.value,method="manhattan"))
    }

  # calculating Eder's Simple distance to a distance matrix
  if(distance.measure == "ES") {
    distance.table = 
       as.matrix(dist(sqrt(table.with.all.freqs[,1:mfw]),method="manhattan"))
    distance.name.on.graph = "Eder's Simple distance"
    distance.name.on.file = "Eder's Simple"
    }

  # calculating Manhattan distance to a distance matrix
  if(distance.measure == "MH") {
    distance.name.on.graph = "Manhattan distance"
    distance.name.on.file = "Manhattan"
    distance.table = 
         as.matrix(dist(table.with.all.freqs[,1:mfw],method="manhattan"))
    }

  # calculating Canberra distance to a distance matrix
  if(distance.measure == "CB") {
    distance.name.on.graph = "Canberra distance"
    distance.name.on.file = "Canberra"
    distance.table = 
         as.matrix(dist(table.with.all.freqs[,1:mfw],method="canberra"))
    }

  # calculating Euclidean distance to a distance matrix
  if(distance.measure == "EU") {
    distance.name.on.graph = "Euclidean distance"
    distance.name.on.file = "Euclidean"
    distance.table = 
         as.matrix(dist(table.with.all.freqs[,1:mfw],method="euclid"))
    }

  # replaces the names of the samples (the extension ".txt" is cut off)
  rownames(distance.table)=gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)",
                        "",rownames(table.with.all.freqs))
  colnames(distance.table)=gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)",
                        "",rownames(table.with.all.freqs))
}





# #################################################
# a tiny module for graph auto-coloring: 
# uses the function "assign.plot.colors()"
# #################################################

names.of.texts = gsub("(\\.txt)||(\\.xml)||(\\.html)||(\\.htm)","",rownames(table.with.all.freqs))

# using an appropriate function to assing colors to subsequent samples
colors.of.pca.graph = assign.plot.colors(labels=names.of.texts,
                            col=colors.on.graphs)





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
    culling.info = paste(culling.min,"-",culling.max,sep="") }

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
          plot(nj(distance.table), font=1, tip.color=colors.of.pca.graph)
        # any other linkage algorithm is produced by hclust()
        } else {
          # clustering the distances stored in the distance.table
          clustered.data = hclust(as.dist(distance.table),method=linkage)
          # reordering the vector of colors to fit the order of clusters
          colors.on.dendrogram = colors.of.pca.graph[clustered.data$order]
          # converting the clusters into common dendrogram format
          tree.with.clusters = as.dendrogram(clustered.data,hang=0)
          # now, preparing the procedure for changing leaves‘ color attributes
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
                  i <- 0
          # adding the attributes to subsequent leaves of the dendrogram,
          # using the above colLab(n) function
          dendrogram.with.colors = dendrapply(tree.with.clusters, colLab)
          # finally, ploting the whole stuff
          plot(dendrogram.with.colors,
          main = graph.title,
          horiz = dendrogram.layout.horizontal) 
          if(dendrogram.layout.horizontal == TRUE) {
                  title(sub=graph.subtitle) 
          } else {
                  title(sub=graph.subtitle, outer=TRUE, line=-1)  
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
  mds.results = cmdscale(distance.table,eig=TRUE)
  # prepare the xy coordinates, add the margins, add the label offset
  xy.coord = mds.results$points[,1:2]
  if(text.id.on.graphs == "both") {
    label.coord = cbind(mds.results$points[,1],(mds.results$points[,2] + (0.01*label.offset*
                      abs(max(mds.results$points[,2]) - min(mds.results$points[,2])))))
    } else {
    label.coord = xy.coord
    }
  plot.area = define.plot.area(mds.results$points[,1],mds.results$points[,2],
                               xymargins=add.to.margins,
                               v.offset=label.offset)
  # define the plotting function needed:
  plot.current.task = function(){ 
    if(text.id.on.graphs == "points" || text.id.on.graphs == "both") {
      plot(xy.coord, type="p", 
           ylab="", xlab="", 
           xlim=plot.area[[1]],ylim=plot.area[[2]],
           main = graph.title,
           sub = graph.subtitle,
           col = colors.of.pca.graph,
           lwd = plot.line.thickness) 
      }
    if(text.id.on.graphs == "labels") {
      plot(xy.coord, type="n", 
           ylab="", xlab="", 
           xlim=plot.area[[1]],ylim=plot.area[[2]],
           main = graph.title,
           sub = graph.subtitle,
           col = colors.of.pca.graph,
           lwd = plot.line.thickness) 
      }
    if(text.id.on.graphs == "labels" || text.id.on.graphs == "both") {
      text(label.coord, rownames(label.coord), col=colors.of.pca.graph) 
      }
    axis(1,lwd=plot.line.thickness)
    axis(2,lwd=plot.line.thickness)
    box(lwd=plot.line.thickness)
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
  expl.var = round(((pca.results$sdev^2)/sum(pca.results$sdev^2)*100),1)
  PC1_lab = paste("PC1 (",expl.var[1],"%)", sep="")
  PC2_lab = paste("PC2 (",expl.var[2],"%)", sep="")

  # prepare the xy coordinates, add the margins, add the label offset
  xy.coord = pca.results$x[,1:2]
  if(text.id.on.graphs == "both") {
    label.coord = cbind(pca.results$x[,1],(pca.results$x[,2] + (0.01*label.offset*
                      abs(max(pca.results$x[,2]) - min(pca.results$x[,2])))))
    } else {
    label.coord = xy.coord
    }
  plot.area = define.plot.area(pca.results$x[,1],pca.results$x[,2],
                               xymargins=add.to.margins,
                               v.offset=label.offset)
  # define the plotting function needed:
  plot.current.task = function(){
    if (pca.visual.flavour == "classic"){
      if(text.id.on.graphs == "points" || text.id.on.graphs == "both") {
        plot(xy.coord,
             type="p",
             xlim=plot.area[[1]],ylim=plot.area[[2]],
             xlab="",ylab=PC2_lab,
             main = graph.title,sub = paste(PC1_lab,"\n",graph.subtitle),
             col=colors.of.pca.graph,
             lwd=plot.line.thickness) 
      }
      if(text.id.on.graphs == "labels") {
        plot(xy.coord,
             type="n",
             xlim=plot.area[[1]],ylim=plot.area[[2]],
             xlab="",ylab=PC2_lab,
             main = graph.title,sub = paste(PC1_lab,"\n",graph.subtitle),
             col=colors.of.pca.graph,
             lwd=plot.line.thickness) 
      }
      abline(h=0, v=0, col = "gray60",lty=2)
      if(text.id.on.graphs == "labels" || text.id.on.graphs == "both") {
        text(label.coord, rownames(pca.results$x), col=colors.of.pca.graph) 
      }
      axis(1,lwd=plot.line.thickness)
      axis(2,lwd=plot.line.thickness)
      box(lwd=plot.line.thickness)
    } else if(pca.visual.flavour == "loadings"){
      biplot(pca.results, 
             col=c("grey70", "black"), 
             cex=c(0.7, 1), xlab="", 
             ylab=PC2_lab, 
             main=paste(graph.title, "\n\n", sep=""), 
             sub=paste(PC1_lab,"\n",graph.subtitle, sep=""),var.axes=FALSE)
    } else if(pca.visual.flavour == "technical"){
      layout(matrix(c(1,2), 2, 2, byrow = TRUE), widths=c(3,1))
      biplot(pca.results, col=c("black", "grey40"), cex=c(1, 0.9), xlab="", ylab=PC2_lab, main=paste(graph.title, "\n\n", sep=""), sub=paste(PC1_lab,"\n",graph.subtitle, sep=""),var.axes=FALSE)
      abline(h=0, v=0, col = "gray60",lty=3)
      # add the subpanel to the right 
      row = mat.or.vec(nc=ncol(pca.results$x),nr=1)
      for (i in 1:ncol(row)){row[,i]<-"grey45"}
      # paint the first two PCS black -- i.e. the ones actually plotted
      row[,1]<-"black"
      row[,2]<-"black"
      barplot(expl.var, col = row, xlab = "Principal components", ylab = "Proportion of variance explained (in %)")
      # set a horizontal dashed line, indicating the psychological 5% barrier  
      abline(h=5, lty=3)
    } else if(pca.visual.flavour == "symbols"){
      # determine labels involved
      labels = c()
      for (c in rownames(pca.results$x)){
        labels = c(labels, gsub("_.*","",c))
      }
      COOR = data.frame(pca.results$x[,1:2], LABEL=labels)
      labels<-c(levels(COOR$LABEL))
      # visualize 
#      library(lattice)
      sps <- trellis.par.get("superpose.symbol")
      sps$pch <- 1:length(labels)
      trellis.par.set("superpose.symbol", sps)
      ltheme <- canonical.theme(color = FALSE)      
      lattice.options(default.theme = ltheme)
      pl<-xyplot(data=COOR, x=PC2~PC1, xlab=paste(PC1_lab,"\n",graph.subtitle, sep=""), ylab=PC2_lab, groups=COOR$LABEL, sub="", key=list(columns=2, text=list(labels), points=Rows(sps, 1:length(labels))),
             panel=function(x, ...){
             panel.xyplot(x, ...)
             panel.abline(v=0, lty=3)
             panel.abline(h=0, lty=3)
      })
      plot(pl)
    }
  }
}
        

# prepares a list of dendrogram-like structures for a bootstrap consensus tree
# (the final tree will be generated later, outside the main loop of the script)
if (analysis.type == "BCT") {
  mfw.info = paste(mfw.min,"-",mfw.max.original, sep="")
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
                                       method=linkage))
  }
########################################################################
  # adds the current dendrogram to the list of all dendrograms
  bootstrap.list[[number.of.current.iteration]] = current.bootstrap.results }

  
# establishing the text to appear on the graph (unless "notitle" was chosen)
if(ngram.size > 1) {
  ngram.value = paste(ngram.size,"-grams", sep="")
  } else {
  ngram.value = "" }
  #
  if(titles.on.graphs == TRUE) {
  graph.title = paste(basename(getwd()),"\n",name.of.the.method)
  if(analysis.type == "BCT") {
  graph.subtitle = paste(mfw.info," MF",toupper(analyzed.features)," ",ngram.value," Culled @ ",culling.info,"%\n",
                    pronouns.info," ",distance.name.on.graph," Consensus ",consensus.strength," ",start.at.info, sep="") 
          } else {
          graph.subtitle = paste(mfw.info," MF",toupper(analyzed.features)," ",ngram.value," Culled @ ",culling.info,"%\n",
                    pronouns.info," ",distance.name.on.graph," ",start.at.info, sep="") }
  } else {
  graph.title = ""
  graph.subtitle = "" }


# name of the output file (strictly speaking: basename) for graphs
graph.filename = paste(basename(getwd()),short.name.of.the.method,mfw.info,
                       "MFWs_Culled",culling.info,pronouns.info,
                       distance.name.on.file,"C",consensus.strength,start.at.info, sep="_")
  if(analysis.type == "BCT") {
    graph.filename = paste(basename(getwd()),short.name.of.the.method,mfw.info,
                       "MFWs_Culled",culling.info,pronouns.info,
                       distance.name.on.file,"C",consensus.strength,start.at.info, sep="_") 
  } else {
    graph.filename = paste(basename(getwd()),short.name.of.the.method,mfw.info,
                       "MFWs_Culled",culling.info,pronouns.info, distance.name.on.file,start.at.info, sep="_") 
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
    pdf(file = paste(graph.filename,"%03d",".pdf",sep=""),
            width=plot.custom.width,height=plot.custom.height,
            pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.jpg.file == TRUE) {
    jpeg(filename = paste(graph.filename,"%03d",".jpg",sep=""), 
            width=plot.custom.width,height=plot.custom.height,
            units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.svg.file == TRUE) {
    svg(filename = paste(graph.filename,"%03d",".svg",sep=""),
            width=plot.custom.width,height=plot.custom.height,
            pointsize=plot.font.size)
    }
  if(write.png.file == TRUE) {
    png(filename = paste(graph.filename,"%03d",".png",sep=""), 
            width=plot.custom.width,height=plot.custom.height,
            units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
}
##################################################


# writing distance table(s) to a file (if an appropriate option has been chosen)
if(save.distance.tables == TRUE && exists("distance.table") == TRUE) {
  distance.table.filename = paste("distance_table_",mfw,"mfw_",current.culling,"c.txt",sep="")
  write.table(file=distance.table.filename, distance.table)
}

# writing the words (or features) actually used in the analysis
if(save.analyzed.features == TRUE) {
  cat(colnames(table.with.all.freqs[,1:mfw]),
     file=paste("features_analyzed_",mfw,"mfw_",current.culling,"c.txt",sep=""),
     sep="\n")
}

# writing the frequency table that was actually used in the analysis
if(save.analyzed.freqs == TRUE) {
  write.table(table.with.all.freqs[,1:mfw],
     file=paste("frequencies_analyzed_",mfw,"mfw_",current.culling,"c.txt",sep=""))
}





##############################################
##############################################
# consensus tree as a network: preparing Gephi input data
##############################################

if(exists("distance.table") == TRUE) {
  distances = distance.table
  # next, we need to create an empty matrix of the same size as the dist table
  connections = matrix(data=0,nrow=length(distances[,1]),ncol=length(distances[1,]))
  # iterate over the rows of dist matrix to retrieve subsequent nearest neighbors
  for(i in 1: length(distances[,1])) {
    # establish a link between two nearest neighbors by assigning 3,
    # 2nd runner-up will get 2, and 3rd runner-up will get 1
    connections[i,(order(distances[i,])[2])] = 3
    connections[i,(order(distances[i,])[3])] = 2
    connections[i,(order(distances[i,])[4])] = 1
  }

all.connections = all.connections + connections
}
##############################################
##############################################








}    # <-- the internal loop for(i) returns here
# #################################################

# blank line on the screen
cat("\n")


}    # <-- the main loop for(j) returns here
# ################################################






######################################################
######################################################
# network analysis: preparing a list of edges

if(exists("distance.table") == TRUE) {
  rownames(all.connections) = rownames(distances)
  colnames(all.connections) = colnames(distances)

  edges=c()
  for(i in 1:(length(all.connections[,1])) ) {
    for(j in 1:(length(all.connections[1,])) ) {
      from = rownames(all.connections)[i]
      to = colnames(all.connections)[j]
      # undirected, i.e. links "to" and "from" are summarized;
      # it means that possible bias is partialy overcome
      weight = all.connections[i,j] + all.connections[j,i]
      # directed: it matters whether a given sample points or is poited
#      weight = all.connections[i,j]
      #
      current.row = c(from, to, weight, "undirected")
      #
      # if there is a connection, record it in a common table
      if(weight > 0) {
        edges = rbind(edges, current.row)
      }
    }
  }

  colnames(edges) = c("Source","Target","Weight","Type")
  rownames(edges) = c(1:length(edges[,1]))
  edges = as.data.frame(edges)

  edges.filename = paste(graph.filename,"EDGES.csv",sep="")
  write.csv(file=edges.filename,quote=F,edges)
}

######################################################
######################################################




# bootstrap visualization
if(analysis.type == "BCT") {

# as above, the task to be plotted is saved as a function
if(length(bootstrap.list) <= 2) {
  cat("\n\nSORRY, BUT YOU ARE EXPECTING TOO MUCH...!\n\n",
  "There should be at least 3 iterations to make a consensus tree\n\n")
  } else {
  plot.current.task = function(){ 
        plot(consensus(bootstrap.list, p=consensus.strength),
           type="u",
           font=1,
           lab4ut="axial", 
           tip.color = colors.of.pca.graph)
        title (main = graph.title)
        title (sub = graph.subtitle) }

# The core code for the graphic output... Yes, you are right: you've seen
# the same lines above. Instead of blaming us, write better code yourself
# and let us know.
  if(display.on.screen == TRUE) {
    plot.current.task()
    }
  if(write.pdf.file == TRUE) {
    pdf(file = paste(graph.filename,"%03d",".pdf",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.jpg.file == TRUE) {
    jpeg(filename = paste(graph.filename,"%03d",".jpg",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.svg.file == TRUE) {
    svg(filename=paste(graph.filename,"%03d",".svg",sep=""), 
         width=plot.custom.width,height=plot.custom.height,
         pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.png.file == TRUE) {
    png(filename = paste(graph.filename,"%03d",".png",sep=""), 
         width=plot.custom.width,height=plot.custom.height,
         units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
}}


# #################################################
# final cleaning




# creating an object (list) that will contain the final results,
# tables of frequencies, etc.etc.
results.stylo = list()
# elements that we want to add on this list
variables.to.save = c("distance.table", "frequencies.0.culling",
                      "table.with.all.freqs", "table.with.all.zscores",
                      "edges")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.stylo[[i]] = get(i)
}





cat("\n")
cat("Some results should have been written into a few files; you should\n")
cat("be able to find them in your current (working) directory. These include\n")
cat("a list of words used to build a table of frequencies, the table itself,\n")
cat("a file containg recent configuration, etc.\n")
cat("Advanced users: you can pipe the results to a variable, e.g.:\n")
cat("    my.results = stylo()\n")
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

# remove the only global variable that was used at some point
#if(exists("colLab") == TRUE) {
#  rm(colLab, envir = globalenv())
#}

# return (tacitly) the value of the function 
invisible(results.stylo)
}
