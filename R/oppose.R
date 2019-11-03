


oppose = function(gui = TRUE,
             path = NULL,
             primary.corpus = NULL,
             secondary.corpus = NULL,
             test.corpus = NULL,
             primary.corpus.dir = "primary_set",
             secondary.corpus.dir = "secondary_set",
             test.corpus.dir = "test_set", ...) {
#






polygons.on.graph = T
naive.bayes = F
svm.classification = F



# a weird way of checking if a corpus has been loaded: variable initiation
corpus.exists = F



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

# loading the default settings as defined in the following function
# (it absorbes the arguments passed from command-line)
variables = stylo.default.settings(...)



# this needs to be deleted sooner or later

stage.I.word.selection = TRUE
stage.II.similarity.test = TRUE




# optionally, displaying a GUI box
# (it absorbes the arguments passed from command-line)
if (gui == TRUE) {
      # first, checking if the GUI can be displayed
      # (the conditional expression is stolen form the generic function "menu")
      if (.Platform$OS.type == "windows" || .Platform$GUI ==
            "AQUA" || (capabilities("tcltk") && capabilities("X11") &&
            suppressWarnings(tcltk::.TkUp))) {
        variables = gui.oppose(...)
      } else {
        message("")
        message("GUI could not be launched -- default settings will be used;")
        message("otherwise please pass your variables as command-line agruments\n")
      }
}





# #############################################################################
# Explicit assignment of all the variables, in order to avoid attach()
# #############################################################################

text.slice.length = variables$text.slice.length
text.slice.overlap = variables$text.slice.overlap
rare.occurrences.threshold = variables$rare.occurrences.threshold
zeta.filter.threshold = variables$zeta.filter.threshold
plot.token = variables$plot.token
oppose.method = variables$oppose.method
display.on.screen = variables$display.on.screen
write.pdf.file = variables$write.pdf.file
write.png.file = variables$write.png.file
use.color.graphs = variables$use.color.graphs
titles.on.graph = variables$titles.on.graph
identify.points = variables$identify.points
classification = variables$classification
visualization = variables$visualization

corpus.format = variables$corpus.format
corpus.lang = variables$corpus.lang
splitting.rule = variables$splitting.rule
preserve.case = variables$preserve.case
sample.size = variables$sample.size
sampling = variables$sampling
sampling.with.replacement = variables$sampling.with.replacement
analyzed.features = variables$analyzed.features
ngram.size = variables$ngram.size

splitting.rule = variables$splitting.rule
preserve.case = variables$preserve.case
encoding = variables$encoding







# grabbed from classify:

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


custom.graph.filename = variables$custom.graph.filename
custom.graph.title = variables$custom.graph.title


# Is custom title requested? If yes, use it
if(is.null(custom.graph.title) == FALSE) {
        # but first, a tiny sanitizing is needed
        graph.title = as.character(custom.graph.title)[1]
} else {
        # otherwise, assign the current working directory name
        graph.title = basename(getwd())
}





# If the tables with frequencies could not loaded so far (for any reason),
# try to load an external corpus (R object) passed as an argument

###############################################################################
# Checking if the argument "training.corpus" and/or "test.corpus" has been used
#
# Iterating over two sets: primary set and secondary set
for(iteration in 1:2) {
    # first iteration: primary set
    if(iteration == 1) {
      parsed.corpus = primary.corpus
    }
    # second iteration: secondary set
    if(iteration == 2) {
      parsed.corpus = secondary.corpus
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






############################################################################
############################################################################


# if pre-processed corpora from R objects could not be loaded, then use files
if(corpus.exists == FALSE) {
 
  # Checking whether required files and subdirectories exist
  # First check: allow user to choose a suitable folder via GUI
  if(file.exists(primary.corpus.dir) == FALSE | file.exists(secondary.corpus.dir) == FALSE) {
    selected.path = tk_choose.dir(caption = "Select your working directory. It should two subdirectories called *primary_set* and *secondary_set*")
    setwd(selected.path)
  }

  # If the user failed to provide a suitable folder at this point, abort.
  if(file.exists(primary.corpus.dir) == FALSE | file.exists(secondary.corpus.dir) == FALSE) {
    message("\n\n", "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "Working directory should contain two subdirectories:
        \"", primary.corpus.dir, "\" and \"", secondary.corpus.dir, "\"\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n", sep = "")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }
  
  # Retrieving the names of samples
  #
  filenames.primary.set = list.files(primary.corpus.dir)
  filenames.secondary.set = list.files(secondary.corpus.dir)
  
  # Checking if the subdirectories contain any stuff
  if(length(filenames.primary.set) <2 | length(filenames.secondary.set) <2) {
    message("\n\n", "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "Both subdirectories \"", primary.corpus.dir, "\" and \"",
        secondary.corpus.dir, "\"\nshould contain at least two text samples!\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n", sep = "")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }

  # loading text files, splitting, parsing, n-gramming, samping, and so forth
  corpus.of.primary.set = load.corpus.and.parse(files = filenames.primary.set,
                         corpus.dir = primary.corpus.dir,
                         encoding = encoding,
                         markup.type = corpus.format,
                         corpus.lang = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         features = analyzed.features,
                         ngram.size = ngram.size)

  # loading text files: test set
  corpus.of.secondary.set = load.corpus.and.parse(files=filenames.secondary.set,
                         corpus.dir = secondary.corpus.dir,
                         encoding = encoding,
                         markup.type = corpus.format,
                         corpus.lang = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         features = analyzed.features,
                         ngram.size = ngram.size)

}





  # We need a list of the most frequent words used in the current corpus,
  # in descendent order, without frequencies (just a list of words).
  wordlist.raw = sort(table( c(unlist(corpus.of.primary.set),
                          unlist(corpus.of.secondary.set))), decreasing=TRUE)


  # we want to filter out rare words (e.g. hapax legomena and/or dislegomena)
  wordlist.raw = wordlist.raw[wordlist.raw > rare.occurrences.threshold]

  # the only thing we need are words ordered by frequency (no frequencies)
  wordlist = names(wordlist.raw)





# blank line on the screen

message("")
message("Slicing the texts into samples...")

primary.slices = make.samples(corpus.of.primary.set,
                              sample.size = text.slice.length,
                              sample.overlap = text.slice.overlap,
                              sampling = "normal.sampling")

secondary.slices = make.samples(corpus.of.secondary.set,
                              sample.size = text.slice.length,
                              sample.overlap = text.slice.overlap,
                              sampling = "normal.sampling")

# ###############################################################





# ###############################################################

message("")
message("Extracting distinctive words... (this might take a while)")




message("")
message("Primary set...")

# iterating over the samples and slices, checking them agains the wordlist
# do.call() is a game-changer here, in terms of the computation time (5x or so)
table.primary.set = do.call(rbind,
             lapply(primary.slices, function(x) as.numeric(wordlist %in% x)))
#table.primary.set = t(table.primary.set)
colnames(table.primary.set) = wordlist

# adding the counts for particular slices, computing percentage of slices
# that contain words from the wordlist
comparison.primary = colSums(table.primary.set)/length(primary.slices)*100
names(comparison.primary) = wordlist



message("Secondary set...\n\n")

# iterating over the samples and slices, checking them agains the wordlist
# do.call() is a game-changer here, in terms of the computation time (5x or so)
table.secondary.set = do.call(rbind,
             lapply(secondary.slices, function(x) as.numeric(wordlist %in% x)))
#table.secondary.set = t(table.secondary.set)
colnames(table.secondary.set) = wordlist

#table.secondary.set = t(table.secondary.set)
colnames(table.secondary.set) = wordlist

# adding the counts for particular slices, computing percentage of slices
# that contain words from the wordlist
comparison.secondary = colSums(table.secondary.set)/length(secondary.slices)*100
names(comparison.secondary) = wordlist


#########################################################################





# combining two vectors of counts: for I and II sets
comparison = cbind(comparison.primary,comparison.secondary)
# accidentally, some 0 might be found in both sets at a time;
# this should not happen, but sometimes it does -- getting rid of it
comparison = comparison[rowSums(comparison) != 0,]

message("comparison done!\n")







#########################################################################
# Finally, we want to save some of the variable values for later use;
# they are automatically loaded into the GUI at the next run of the script.
cat("", file = "oppose_config.txt", append = FALSE)
var.name = function(x) {
    if(is.character(x) == TRUE) {
        cat(paste(deparse(substitute(x)), " = \"", x, "\"", sep = ""), file = "oppose_config.txt", sep = "\n", append = TRUE)
    } else {
        cat(paste(deparse(substitute(x)), x, sep = " = "), file = "oppose_config.txt", sep = "\n", append = TRUE) }
    }
var.name(text.slice.length)
var.name(text.slice.overlap)
var.name(rare.occurrences.threshold)
var.name(zeta.filter.threshold)
var.name(oppose.method)
var.name(display.on.screen)
var.name(write.pdf.file)
var.name(write.png.file)
var.name(use.color.graphs)
var.name(titles.on.graph)
var.name(identify.points)
var.name(visualization)
var.name(classification)
var.name(plot.token)
#########################################################################







##########################################################################

# boxplot
present_in_primary = 0
present_in_secondary = 0
if (oppose.method == "box.plot"){
        counts = c()
        categories = c()
        # make the boxplot
        # primary slices
        for (slice in primary.slices){
                token_hit_counter = 0
                for (t in slice){
                        if (t == plot.token){
                                token_hit_counter = token_hit_counter+1
                        }
                }
                if (token_hit_counter > 0){
                        present_in_primary = present_in_primary+1
                }
                counts = c(counts, token_hit_counter)
                categories = c(categories, "Primary")
        }
        # secondary slices
        for (slice in secondary.slices){
                token_hit_counter = 0
                for (t in slice){
                        if (t == plot.token){
                                token_hit_counter = token_hit_counter+1
                        }
                }
                if (token_hit_counter > 0){
                        present_in_secondary = present_in_secondary+1
                }
                counts = c(counts, token_hit_counter)
                categories = c(categories, "Secondary")
        }

        graph.title = paste('Boxplot for \"', plot.token, '\"', sep="")

        plot.current.task = function(){
                boxplot.matrix = data.frame(COUNT=counts, CATEGORY=categories)
                name_prim = paste("Primary (", present_in_primary, "/", length(primary.slices), ")", sep="")
                name_sec = paste("Secondary (", present_in_secondary, "/", length(secondary.slices), ")", sep="")
                boxplot(COUNT~CATEGORY, data=boxplot.matrix, ylab=paste("Absolute frequency per slice (", text.slice.length, " words)", sep=""), names=c(name_prim, name_sec))
                prim.vals = boxplot.matrix[boxplot.matrix$CATEGORY=="Primary",1]
                sec.vals = boxplot.matrix[boxplot.matrix$CATEGORY=="Secondary",1]
                wilcox.result <- wilcox.test(x=prim.vals, y=sec.vals)
                print(wilcox.result)
                if (as.numeric(wilcox.result$p.value) < 0.05){
                        title(main=graph.title, sub="(Wilcoxon rank sum: p < 0.05)\n")
                } else {
                        title(main=graph.title, sub="(Wilcoxon rank sum: p > 0.05)\n")
                }
        }
        if(display.on.screen == TRUE) {
                plot.current.task()
        }
        if(write.pdf.file == TRUE) {
                pdf(file = paste("boxplot_", plot.token, "%03d",".pdf",sep=""),pointsize=10)
                plot.current.task()
                dev.off()
        }
        if(write.png.file == TRUE) {
                png(filename = paste("boxplot_", plot.token,"%03d",".png",sep=""),width=7,height=7,units="in",res=300,pointsize=10)
                plot.current.task()
                dev.off()
        }
}

# mann.whitney / wilcoxon (see A. Kilgariff, Comparing Corpora. "International Journal of Corpus Linguistics" 6(1):1-37)
if (oppose.method == "mann.whitney"){
        message("performing Wilcoxon/Mann-Whitney test: try to be patient...\n")
        long.method.name = "Wilcoxon | Mann-Whitney"
        short.method.name = "Wilcox"
        statistics = c()
        all_tokens = c()
        for (slice in primary.slices){all_tokens = union(all_tokens, slice)}
        all_tokens = sort(all_tokens)
        # primary slices
        statistics = c()
        # loop through tokens
        for (token in all_tokens){
                message(token)
                # collect freqs in primary slices
                primary_counts = c()
                for (slice in primary.slices){primary_counts = c(primary_counts, sum(slice==token))}
                # collect freqs in secondary slices
                secondary_counts = c()
                for (slice in secondary.slices){secondary_counts = c(secondary_counts, sum(slice==token))}
                # get statistic if relevant:
                if ( sum(primary_counts)+sum(secondary_counts) >= rare.occurrences.threshold){
                        wilcox.stat = wilcox.test(primary_counts, secondary_counts, exact=F, )$statistic
                        statistics[token] = wilcox.stat
                }
        }
        statistics=sort(statistics)
        max_stat = max(statistics)
        # get tail percentage
        tail = round(length(statistics)/100*zeta.filter.threshold)
        top_words = statistics[0:tail]
        print(top_words)
        tail_words = rev(statistics[(length(statistics)-tail):length(statistics)])
        print(tail_words)

words.preferred.by.primary.author = names(top_words)
words.avoided.by.primary.author = names(tail_words)

}




# Craig's zeta
#####################################################################
if(oppose.method == "craig.zeta") {
        long.method.name="Craig's Zeta"
        short.method.name="Craig"
        # applying an appropriate function
        zeta.results = zeta.craig(comparison, zeta.filter.threshold)
        # extracting two elements from the obtained list
        words.preferred = zeta.results$words_preferred
        words.avoided = zeta.results$words_avoided
}


# Eder's zeta (inspired by Canberra distance measure)
#####################################################################
if(oppose.method == "eder.zeta") {
        long.method.name="Eder's Zeta"
        short.method.name="Eder"
        # applying an appropriate function
        zeta.results = zeta.eder(comparison, zeta.filter.threshold)
        # extracting two elements from the obtained list
        words.preferred = zeta.results$words_preferred
        words.avoided = zeta.results$words_avoided
}


# Zeta based on chi-square test
#####################################################################
if(oppose.method == "chisquare.zeta") {
        long.method.name="Chi-square Zeta"
        short.method.name="Chi-sq"
        # applying an appropriate function
        zeta.results = zeta.chisquare(comparison)
        # extracting two elements from the obtained list
        words.preferred = zeta.results$words_preferred
        words.avoided = zeta.results$words_avoided
}




if (oppose.method != "box.plot"){
        words.preferred.by.primary.author = names(words.preferred)
        words.avoided.by.primary.author = names(words.avoided)
}



########################################################################
# extracted words will be written to output files

if (oppose.method != "box.plot"){
# some comments into the file containing wordlist
cat("# The file contains words that were extracted in the oppose script:",
  "# this subset lists words significantly PREFERRED by primary author(s).",
  "# The list can be used as an input wordlist for other methods, and for this",
  "# purpose it can be manually revised, edited, deleted, culled, etc.",
  "# You can either delete unwanted words, or mark them with \"#\"",
  "# -----------------------------------------------------------------------",
  "",
      file = "words_preferred.txt", sep = "\n")
# the current wordlist into a file
    # checking if encoding conversion is needed
    if(encoding == "native.enc") {
      data.to.be.saved = words.preferred.by.primary.author
    } else {
      data.to.be.saved = iconv(words.preferred.by.primary.author, to=encoding)
    }
# writing the stuff
cat(data.to.be.saved, file = "words_preferred.txt", sep = "\n", append = TRUE)
#
#
#


# some comments into the file containing worlist
cat("# The file contains words that were extracted in Burrows' Zeta test:",
  "# this subset lists words significantly AVOIDED by primary author(s).",
  "# The list can be used as an input wordlist for other methods, and for this",
  "# purpose it can be manually revised, edited, deleted, culled, etc.",
  "# You can either delete unwanted words, or mark them with \"#\"",
  "# -----------------------------------------------------------------------",
  "",
      file = "words_avoided.txt", sep = "\n")
# the current wordlist into a file
    # checking if encoding conversion is needed
    if(encoding == "native.enc") {
      data.to.be.saved = words.avoided.by.primary.author
    } else {
      data.to.be.saved = iconv(words.avoided.by.primary.author, to=encoding)
    }
# writing the stuff
cat(data.to.be.saved, file = "words_avoided.txt", sep = "\n", append = TRUE)
#
#
}
#} # <---- the first stage of the analysis is completed









# plotting functionality:

# first, checking if anything can be plotted
if (visualization == "words" | visualization == "markers"){
        if(length(words.avoided.by.primary.author) +
        length(words.preferred.by.primary.author) < 10) {
                message("there is not enough discriminative words to continue,\n",
                "either the filter threshold is too strong, or the sample",
                " size too small\n", "  number of preferred words:\t",
                length(words.preferred.by.primary.author), "\n",
                "  number of avoided words:\t",
                length(words.avoided.by.primary.author), sep = "")
        visualization = "none"
        stage.II.similarity.test = FALSE
        }
}





if (visualization == "words" && oppose.method != "box.plot"){

        # only a portion of discinctive words (e.g. 70) will be plotted
        if(length(names(words.preferred)) > 70) {
                preferred.words.for.plotting = names(words.preferred)[1:70]
                preferred.indices.for.plotting = 1:70
                preferred.scores.for.plotting = words.preferred[1:70]
        } else {
                preferred.words.for.plotting = names(words.preferred)
                preferred.indices.for.plotting = 1:length(words.preferred)
                preferred.scores.for.plotting = words.preferred
        }
        # the same procedure applied to the avoided words
        if(length(names(words.avoided)) > 70) {
                avoided.words.for.plotting = names(words.avoided)[1:70]
                avoided.indices.for.plotting = 1:70
                avoided.scores.for.plotting = words.avoided[1:70]
        } else {
                avoided.words.for.plotting = names(words.avoided)
                avoided.indices.for.plotting = 1:length(words.avoided)
                avoided.scores.for.plotting = words.avoided
        }




        plot.current.task = function(){
                plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(-1,1), type="n", xlab="Rank of the item", ylab="Score")
                text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7, srt=90, adj=c(0,0))
                text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7, srt=90, adj=c(1,0))
                abline(h=0, lty=2)
                mtext("Preferred", side = 4, at = 0.5, las = 3)
                mtext("Avoided", side = 4, at = -0.5)
                title(main = graph.title)
        }

        if(titles.on.graph == TRUE) {
                graph.title = paste(graph.title,"\n",long.method.name)
        } else {
        graph.title = ""}
        if(display.on.screen == TRUE){
                plot.current.task()
        }
        # check if a custom filename has been set
        if(is.character(custom.graph.filename) == TRUE &
               length(custom.graph.filename) > 0) {
          # if a custom file name exists, then use it
          graph.filename = custom.graph.filename
        } else {
          graph.filename <- paste(basename(getwd()),short.method.name, sep="_")
        }
        if(write.png.file == TRUE) {
                png(filename = paste(graph.filename,"%03d.png",sep="_"),
                width=7,height=7,res=300, units="in")
                plot.current.task()
        dev.off()}
        if(write.pdf.file == TRUE) {
                pdf(file = paste(graph.filename,"%03d.pdf",sep="_"))
                plot.current.task()
        dev.off()}
}
# end of plotting functionality









### II stage ######################################################
if (stage.II.similarity.test == TRUE && oppose.method != "box.plot") {
# Note: it is impossible to perform stage II when the box.plot method is used

# checking if the test set exists and if it contains file(s)
        if(file.exists(test.corpus.dir) == TRUE) {
                if (length(dir(test.corpus.dir)) > 0) {

# retrieving the remaining names of samples
filenames.test.set = list.files(test.corpus.dir)
#


  corpus.of.test.set = load.corpus.and.parse(files=filenames.test.set,
                         corpus.dir = test.corpus.dir,
                         encoding = encoding,
                         markup.type = corpus.format,
                         corpus.lang = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         features = analyzed.features,
                         ngram.size = ngram.size)


  test.slices = make.samples(corpus.of.test.set,
                              sample.size = text.slice.length,
                              sample.overlap = text.slice.overlap,
                              sampling = "normal.sampling")

#
# blank line on the screen
message("")
#
}
} else {
message("No test set samples found\n",
    "Performing a simple comparison of the training samples...")
}
#
#
#



message("")


# variable initiation
summary.zeta.scores = c()


# checking if the test set exists and if it contains file(s)
# depending on the ansewer, initializing 2 or 3 turns of the loop
if(file.exists(test.corpus.dir) == TRUE) {
    if(length(dir(test.corpus.dir)) > 0) {
    loop.size = 3 }
} else {
    loop.size = 2
}

# loop for (1) primary set and (2) secondary set
for(i in 1 : loop.size) {

        labels = c("primary","secondary","unknown")

        if(i == 1) {
                current.corpus = primary.slices
        }
        if(i == 2) {
                current.corpus = secondary.slices
        }
        if(i == 3) {
                current.corpus = test.slices
        }



        preferred = lapply(current.corpus, function(x) as.numeric(words.preferred.by.primary.author %in% x))
        avoided = lapply(current.corpus, function(x) as.numeric(words.avoided.by.primary.author %in% x))
        y.coord = unlist(lapply(preferred, function(x) {sum(x)/length(x)*100}))
        x.coord = unlist(lapply(avoided, function(x) {sum(x)/length(x)*100}))
        current.corpus.scores = cbind(x.coord, y.coord, labels[i])

        summary.zeta.scores = rbind(summary.zeta.scores,current.corpus.scores)

}


# making the matrix appropriately shaped, namely:
# converting the matrix into a table, making the numbers numeric again
colnames(summary.zeta.scores) = c("preferred","avoided","class")
#summary.zeta.scores = as.data.frame(summary.zeta.scores, stringsAsFactors=FALSE)
#summary.zeta.scores[,1] = as.numeric(summary.zeta.scores[,1])
#summary.zeta.scores[,2] = as.numeric(summary.zeta.scores[,2])
#summary.zeta.scores[,3] = as.factor(summary.zeta.scores[,3])


} # <---- the second stage of the analysis is completed


### III stage ######################################################
if ((visualization == "markers") && (oppose.method != "box.plot")){
  # a tiny module for graph auto-coloring (copied from "Delta test 0.4.1")
  # NOTE: in ver. 0.4.8, this module has been turned to R function, and improved
  #
  if(use.color.graphs == TRUE) {
  color.numeric.values = c(1)
  current.color = 1
  names.of.the.texts = rownames(summary.zeta.scores)
  #
  available.colors = rep(c("red","green","blue","black","orange","purple",
                           "darkgrey","brown","maroon4","mediumturquoise",
                           "gold4", "deepskyblue","yellowgreen","grey",
                           "chartreuse4", "khaki", "navy", "palevioletred",
                           "greenyellow", "darkolivegreen4", "chocolate4"
                           ),10)
  #
  for(w in 2:length(names.of.the.texts)) {
      if(gsub("_.*","",names.of.the.texts)[w]
         %in%
         gsub("_.*","",names.of.the.texts[1:(w-1)]) == TRUE) {
         find.color = which(gsub("_.*","",names.of.the.texts) ==
                               gsub("_.*","",names.of.the.texts)[w])[1]
         current.color = color.numeric.values[find.color]
         }
         else {
         current.color = max(color.numeric.values) + 1
         }
    color.numeric.values = c(color.numeric.values, current.color)
    }
  colors.of.pca.graph = available.colors[c(color.numeric.values)]
    }
    else {
  colors.of.pca.graph = "black"
  }
  # ########################################
  plot.current.task = function(){
    par(mar=c(4, 4, 4, 7)+.1, xpd=TRUE)
    plot(summary.zeta.scores[,1:2],
         col = colors.of.pca.graph,
         pch = as.numeric(as.factor(summary.zeta.scores[,3])),
         xlab="antimarkers",
         ylab="markers"
         )
    legend("right", unique(gsub("_.*","",rownames(summary.zeta.scores))),
        bty="n", text.col=unique(colors.of.pca.graph),
        fill=unique(colors.of.pca.graph), border="white", inset=-0.27)
    title(main=graph.title)
    par(xpd=F)
    if(polygons.on.graph == TRUE) {
      draw.polygons(summary.zeta.scores)
    }
  }
  if(titles.on.graph == TRUE) {
    graph.title = paste(graph.title,"\n",long.method.name)
    } else {
    graph.title = ""}



  if(display.on.screen == TRUE){
  plot.current.task()
  }



  # check if a custom filename has been set
  if(is.character(custom.graph.filename) == TRUE &
           length(custom.graph.filename) > 0) {
      # if a custom file name exists, then use it
      graph.filename = custom.graph.filename
  } else {
    graph.filename = paste(basename(getwd()),short.method.name, sep="_")
  }


  if(write.png.file == TRUE) {
  png(filename = paste(graph.filename,"%03d.png",sep="_"),
           width=7,height=7,res=300, units="in")
  plot.current.task()
  dev.off()}
  if(write.pdf.file == TRUE) {
  pdf(file = paste(graph.filename,"%03d.pdf",sep="_"))
  plot.current.task()
  dev.off()}
}

### classification ######################################################

if (classification == TRUE && oppose.method != "box.plot") {
  training.subset = length(grep("(primary)|(secondary)",summary.zeta.scores[,3]))
  test.subset = length(summary.zeta.scores[,3])
  my.data = as.data.frame(summary.zeta.scores)
  #################################################################
  # naive Bayes classification
  if(naive.bayes == TRUE) {
#  library(e1071)
    model = naiveBayes(class ~ preferred + avoided, my.data[1:training.subset,])
    classification.results = predict(model, my.data[(training.subset+1):test.subset,-3], type="raw")
    final.probabilities = round(classification.results,2)
    plot(final.probabilities[,2],type="h")
    }
  #################################################################
  # support vector machines
  if(svm.classification == TRUE) {
#    library(e1071)
    model = svm(as.factor(class) ~ preferred + avoided, my.data[1:training.subset,],probability=T)
    ### plot(model, my.data[1:training.subset,])
    #classification.results = predict(model, my.data[(training.subset+1):test.subset,-3], probability=T)
    classification.results = predict(model, my.data[,-3], probability=T)
    classification.results = attr(classification.results,"probabilities")
    final.probabilities = round(classification.results,2)
    # plotting the test set
    #plot(final.probabilities[,2],type="h")
    # plotting all the samples, distinguished by color:
    if (visualization == "markers"){
      plot(final.probabilities[,2],type="h",
            col=c(rep(2,training.subset),rep(4,test.subset-training.subset)))
      }
    }
  #################################################################
  # decision trees
#  if(decision.tree.classification == TRUE) {
#    library(party)
#    model = ctree(as.factor(class) ~ ., my.data[1:training.subset,])
#    classification.results = predict(model, my.data[(training.subset+1):test.subset,-3])
#    }
}







# #################################################
# praparing final resutls: building a class


# renaming some of the variables (for the sake of attractiveness)
if(exists("words.preferred")) {
  words.preferred.scores = words.preferred
}
if(exists("words.avoided")) {
  words.avoided.scores = words.avoided
}
if(exists("words.preferred.by.primary.author")) {
  words.preferred = words.preferred.by.primary.author
}
if(exists("words.avoided.by.primary.author")) {
  words.avoided = words.avoided.by.primary.author
}

# some fake output
variable.to.be.done = c(0,0,0,0)
yet.another.variable = "nothing to be shown"




if(exists("words.preferred")) {
  attr(words.preferred, "description") = "features (words) preferred in the primary set"
  class(words.preferred) = "stylo.data"
}
if(exists("words.avoided")) {
  attr(words.avoided, "description") = "features (words) avoided in the primary set"
  class(words.avoided) = "stylo.data"
}
if(exists("summary.zeta.scores")) {
  attr(summary.zeta.scores, "description") = "zeta scores"
  class(summary.zeta.scores) = "stylo.data"
}
if(exists("classification.results")) {
  attr(classification.results, "description") = "so far, there's nothing here"
}
if(exists("words.preferred.scores")) {
  attr(words.preferred.scores, "description") = "Zeta scores for the preferred words"
}
if(exists("words.avoided.scores")) {
  attr(words.avoided.scores, "description") = "Zeta scores for the avoided words"
}








# Creating an object (list) that will contain the final results,
# including the preferred and avoided words
# This list will be turned into the class "stylo.results"
results.oppose = list()
# elements that we want to add on this list
variables.to.save = c("words.preferred",
                      "words.avoided",
                      "words.avoided.scores",
                      "words.preferred.scores",
                      "summary.zeta.scores",
                      "comparison","primary.slices","secondary.slices","test.slices",
                      "classification.results")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.oppose[[i]] = get(i)
}



# adding some information about the current function call
# to the final list of results
results.oppose$call = match.call()
results.oppose$name = call("oppose")


# This assings the list of final results to the class "stylo.resutls";
# the same class will be used to handle the output of classify(),
# rolling.delta() and oppose(). See the files "print.stylo.results.R"
# and "summary.stylo.results.R" (no help files are provided, since
# these two functions are not visible for the users).
class(results.oppose) <- "stylo.results"

# return the value of the function
return(results.oppose)
}
