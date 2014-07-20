


oppose <-
function(gui = TRUE, 
         path = NULL,
         primary.corpus.dir = "primary_set",
         secondary.corpus.dir = "secondary_set",
         test.corpus.dir = "test_set", ...) {

                 
                 
                 

polygons.on.graph = F
naive.bayes = F
svm.classification = F




                 
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
  cat("using current directory...\n")
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
            suppressWarnings(tcltk:::.TkUp))) {
        variables = gui.oppose(...)
      } else {
        cat("\n")
        cat("GUI could not be launched -- default settings will be used;\n")
        cat("otherwise please pass your variables as command-line agruments\n")
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






# FUNCTIONS:


# #################################################
# Function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). Alternatively, 
# you can write here another rule for splitting.
# Required argument: name of the text to be split
# #################################################

split.sample = function(input.text) {
  # loading the file, splitting into pieces specified by regular expression;
  # here, all sequences between non-letter characters are assumed to be words:
    if(Sys.info()[["sysname"]] == "Windows") { 
	### Windows
	tokenized.text = c(unlist(strsplit(input.text, "\\W+|_+",perl=T)))
	} else {
	### Linux, Mac
	tokenized.text = c(unlist(strsplit(input.text, "[^[:alpha:]]+")))
	}
  tokenized.text = tokenized.text[grep("[^[:digit:]]",tokenized.text)]
}






############################################################################
############################################################################



  # Retrieving the names of samples
  #
  filenames.primary.set = list.files(primary.corpus.dir)
  filenames.secondary.set = list.files(secondary.corpus.dir)

  # Checking whether required files and subdirectories exist
  if(file.exists(primary.corpus.dir) == FALSE | file.exists(secondary.corpus.dir) == FALSE) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "Working directory should contain two subdirectories: 
        \"",primary.corpus.dir,"\" and \"",secondary.corpus.dir,"\"\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
    # back to the original working directory
    setwd(original.path)
    # error message
    stop("corpus prepared incorrectly")
  }
  # Checking if the subdirectories contain any stuff
  if(length(filenames.primary.set) <2 | length(filenames.secondary.set) <2) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
        "Both subdirectories \"",primary.corpus.dir,"\" and \"",
        secondary.corpus.dir,"\" should contain at least two text samples!\n",
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",sep="")
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
                         corpus.dir = secondary.corpus.dir,
                         encoding = encoding,
                         markup.type = corpus.format,
                         language = corpus.lang,
                         splitting.rule = splitting.rule,
                         preserve.case = preserve.case,
                         sample.size = sample.size,
                         sampling = sampling,
                         sampling.with.replacement = sampling.with.replacement,
                         features = analyzed.features,
                         ngram.size = ngram.size)




  # We need a list of the most frequent words used in the current corpus, 
  # in descendent order, without frequencies (just a list of words).
  wordlist.raw = sort(table( c(unlist(corpus.of.primary.set),
                          unlist(corpus.of.secondary.set))), decreasing=TRUE)


  # we want to filter out rare words (e.g. hapax legomena and/or dislegomena)
  wordlist.raw = wordlist.raw[wordlist.raw > rare.occurrences.threshold]

  # the only thing we need are words ordered by frequency (no frequencies)
  wordlist = names(wordlist.raw)

  cat(length(wordlist),"\n\n\n\n")

  
  

# blank line on the screen
cat("\n")


cat("\n")
cat("Slicing the texts into samples...\n\n")

primary.slices = make.samples(corpus.of.primary.set, 
                              sample.size = text.slice.length,
                              sample.overlap = text.slice.overlap, 
                              sampling = "normal.sampling")
                              
secondary.slices = make.samples(corpus.of.secondary.set, 
                              sample.size = text.slice.length,
                              sample.overlap = text.slice.overlap, 
                              sampling = "normal.sampling")



# ###############################################################

cat("\n")
cat("Extracting distinctive words... (this might take a while)\n\n")

# ###############################################################

cat("\n")
cat("Extracting distinctive words... (this might take a while)\n\n")




cat("\n")
cat("Primary set...\n")

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




cat("\n")
cat("Secondary set...\n")

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





cat("comparison done!\n\n")





#########################################################################
# Finally, we want to save some of the variable values for later use;
# they are automatically loaded into the GUI at the next run of the script.
	cat("",file="oppose_config.txt",append=F)
	var.name<-function(x) { 
		if(is.character(x)==TRUE) {
			cat(paste(deparse(substitute(x)),"=\"",x,"\"", sep=""),file="oppose_config.txt",sep="\n",append=T)
        } else {
			cat(paste(deparse(substitute(x)),x, sep="="),file="oppose_config.txt",sep="\n",append=T) }
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






#########################################################################

comparison = cbind(comparison.primary,comparison.secondary)

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
	long.method.name="Wilcoxon | Mann-Whitney"
	short.method.name="Wilcox"
	statistics = c()
	all_tokens = c()
	for (slice in primary.slices){all_tokens = union(all_tokens, slice)}
	all_tokens = unique(all_tokens)
	# primary slices
	statistics = c()
	# loop through tokens
	for (token in all_tokens){
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
	
	if (visualization == "words"){
		# plotting functionality
		preferred.words.for.plotting = c()
		preferred.indices.for.plotting = c()
		preferred.scores.for.plotting = c()
		for (i in 1:length(names(top_words))){
			preferred.words.for.plotting = c(preferred.words.for.plotting, names(top_words)[i])
			preferred.indices.for.plotting = c(preferred.indices.for.plotting, i)
			preferred.scores.for.plotting = c(preferred.scores.for.plotting, top_words[[i]]/max_stat)	
		}

		avoided.words.for.plotting = c()
		avoided.indices.for.plotting = c()
		avoided.scores.for.plotting = c()
		for (i in 1:length(names(tail_words))){
			avoided.words.for.plotting = c(avoided.words.for.plotting, names(tail_words)[i])
			avoided.indices.for.plotting = c(avoided.indices.for.plotting, i)
			avoided.scores.for.plotting = c(avoided.scores.for.plotting, tail_words[[i]]/max_stat)	
		}

		plot.current.task = function(){
			plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(0,1), type="n", xlab="Rank of the item", ylab="Score")
			text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7)
			text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7)
			abline(h=0.5, lty=2)	
			mtext("Preferred", side=4, at=0.75, las=3)
			mtext("Avoided", side=4, at=0.25)
			title(main=graph.title)
		}

		if(titles.on.graph == TRUE) {
			graph.title = paste(basename(getwd()),"\n",long.method.name)
		} else {
			graph.title = ""}	 
		if(display.on.screen == TRUE){
			plot.current.task()
		}
		graph.filename <- paste(basename(getwd()),short.method.name, sep=" ")
		if(write.png.file == TRUE) {
			png(filename = paste(graph.filename,"%03d.png",sep=" "), 
				width=7,height=7,res=300, units="in")
			plot.current.task()
			dev.off()}
		if(write.pdf.file == TRUE) {
			pdf(file = paste(graph.filename,"%03d.pdf",sep=" "))
			plot.current.task()
			dev.off()}
	}
# end of plotting functionality
words.preferred.by.primary.author = names(top_words)
words.avoided.by.primary.author = names(tail_words)

}

# Craig's zeta
#####################################################################

if(oppose.method == "craig.zeta") {
long.method.name="Craig's Zeta"
short.method.name="Craig"
# two ways of expressing the same thing:
differences = ( comparison.primary - comparison.secondary ) / 100 + 1 # note: add-one smoothing
#differences = (comparison.primary + (100 - comparison.secondary )) / 100

# plotting functionality:
if (visualization == "words" && oppose.method != "box.plot"){
words.preferred.by.primary.author = sort(differences[differences > 1 + zeta.filter.threshold],decreasing=TRUE)
words.avoided.by.primary.author = sort(differences[differences < 1 - zeta.filter.threshold])

preferred.words.for.plotting = c()
preferred.indices.for.plotting = c()
preferred.scores.for.plotting = c()
for (i in 1:length(names(words.preferred.by.primary.author))){
	preferred.words.for.plotting = c(preferred.words.for.plotting, names(words.preferred.by.primary.author)[i])
	preferred.indices.for.plotting = c(preferred.indices.for.plotting, i)
	preferred.scores.for.plotting = c(preferred.scores.for.plotting, words.preferred.by.primary.author[[i]])	
}
	
avoided.words.for.plotting = c()
avoided.indices.for.plotting = c()
avoided.scores.for.plotting = c()
for (i in 1:length(names(words.avoided.by.primary.author))){
	avoided.words.for.plotting = c(avoided.words.for.plotting, names(words.avoided.by.primary.author)[i])
	avoided.indices.for.plotting = c(avoided.indices.for.plotting, i)
	avoided.scores.for.plotting = c(avoided.scores.for.plotting, words.avoided.by.primary.author[[i]])	
}

plot.current.task = function(){
	plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(0,2), type="n", xlab="Rank of the item", ylab="Score")
	text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7)
	text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7)
	abline(h=1, lty=2)	
	mtext("Preferred", side=4, at=1.5, las=3)
	mtext("Avoided", side=4, at=0.5)
	title(main=graph.title)
	}

if(titles.on.graph == TRUE) {
	graph.title = paste(basename(getwd()),"\n",long.method.name)
} else {
	graph.title = ""}	 
if(display.on.screen == TRUE){
	plot.current.task()
}
graph.filename <- paste(basename(getwd()),short.method.name, sep=" ")
if(write.png.file == TRUE) {
	png(filename = paste(graph.filename,"%03d.png",sep=" "), 
	width=7,height=7,res=300, units="in")
	plot.current.task()
	dev.off()}
if(write.pdf.file == TRUE) {
	pdf(file = paste(graph.filename,"%03d.pdf",sep=" "))
	plot.current.task()
	dev.off()}
}
# end of plotting functionality
	
words.preferred.by.primary.author = names(sort(differences[differences > 1 
                                    + zeta.filter.threshold],decreasing=TRUE))
words.avoided.by.primary.author = names(sort(differences[differences < 1 
                                    - zeta.filter.threshold]))
}


# Eder's zeta (inspired by Canberra distance measure)
#####################################################################

if(oppose.method == "eder.zeta") {
long.method.name="Eder's Zeta"
short.method.name="Eder"
# computation of the differences between occurences of words
differences = ( comparison.primary - comparison.secondary ) / ( comparison.primary + comparison.secondary )
differences = differences[differences > -1]
differences = differences[differences < 1]

# extracting the distinctive words
words.preferred.by.primary.author = names(sort(differences[differences > 0 + zeta.filter.threshold], decreasing=TRUE))
words.avoided.by.primary.author = names(sort(differences[differences < 0 - zeta.filter.threshold]))

# plotting functionality:
	if (visualization == "words"){
		
		words.preferred.by.primary.author = sort(differences[differences > 0 + zeta.filter.threshold],decreasing=TRUE)
		words.avoided.by.primary.author = sort(differences[differences < 0 - zeta.filter.threshold])
		
		preferred.words.for.plotting = c()
		preferred.indices.for.plotting = c()
		preferred.scores.for.plotting = c()
		for (i in 1:length(names(words.preferred.by.primary.author))){
			preferred.words.for.plotting = c(preferred.words.for.plotting, names(words.preferred.by.primary.author)[i])
			preferred.indices.for.plotting = c(preferred.indices.for.plotting, i)
			preferred.scores.for.plotting = c(preferred.scores.for.plotting, words.preferred.by.primary.author[[i]])	
		}
		
		avoided.words.for.plotting = c()
		avoided.indices.for.plotting = c()
		avoided.scores.for.plotting = c()
		for (i in 1:length(names(words.avoided.by.primary.author))){
			avoided.words.for.plotting = c(avoided.words.for.plotting, names(words.avoided.by.primary.author)[i])
			avoided.indices.for.plotting = c(avoided.indices.for.plotting, i)
			avoided.scores.for.plotting = c(avoided.scores.for.plotting, words.avoided.by.primary.author[[i]])	
		}
		
		plot.current.task = function(){
			plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(-1,1), type="n", xlab="Rank of the item", ylab="Score")
			text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7)
			text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7)
			abline(h=0, lty=2)	
			mtext("Preferred", side=4, at=0.5, las=3)
			mtext("Avoided", side=4, at=-0.5)
			title(main=graph.title)
		}
		
		if(titles.on.graph == TRUE) {
			graph.title = paste(basename(getwd()),"\n",long.method.name)
		} else {
			graph.title = ""}	 
		if(display.on.screen == TRUE){
			plot.current.task()
		}
		graph.filename <- paste(basename(getwd()),short.method.name, sep=" ")
		if(write.png.file == TRUE) {
			png(filename = paste(graph.filename,"%03d.png",sep=" "), 
				width=7,height=7,res=300, units="in")
			plot.current.task()
			dev.off()}
		if(write.pdf.file == TRUE) {
			pdf(file = paste(graph.filename,"%03d.pdf",sep=" "))
			plot.current.task()
			dev.off()}
	}
# end of plotting functionality
words.preferred.by.primary.author = names(sort(differences[differences > 1 + zeta.filter.threshold],decreasing=TRUE))
words.avoided.by.primary.author = names(sort(differences[differences < 1 - zeta.filter.threshold]))
}

# Zeta based on chi-square test
#####################################################################

if(oppose.method == "chisquare.zeta") {
long.method.name="Chi-square Zeta"
short.method.name="Chi-sq"
differences = comparison[,1] - comparison[,2]
positive.values = differences[differences > 0]
negative.values = differences[differences < 0]

# selecting positive differences (=words preferred by primary author)
positive.differences = comparison[names(positive.values),]
# selecting negative differences (=words avoided by primary author)
negative.differences = comparison[names(negative.values),]

# preferred words
b = NULL
for(i in 1: length(positive.differences[,1])) {
	a = chisq.test(c(positive.differences[i,1],positive.differences[i,2]))$p.value
	b = c(b, a)
}
names(b) = names(positive.values)

# displays a sorted list of discriminative words (p<.05)
words.preferred.by.primary.author = names(sort(b[b<0.05]))
words.preferred = b[b<0.05]

# avoided words
b = NULL
for(i in 1: length(negative.differences[,1])) {
  a = chisq.test(c(negative.differences[i,1],negative.differences[i,2]))$p.value
  b = c(b, a)
}
names(b) = names(negative.values)

# displays sorted list of discriminative words (p<.05)
words.avoided.by.primary.author = names(sort(b[b<0.05]))
words.avoided = b[b<0.05]

words.preferred.stats = NULL
for (name in names(words.preferred)){
	stat = chisq.test(c(positive.differences[name,1],positive.differences[name,2]))$statistic
	words.preferred.stats = c(words.preferred.stats, stat)
}
names(words.preferred.stats) = names(words.preferred)
words.preferred.stats = sort(words.preferred.stats, decreasing=T)
	
words.avoided.stats = NULL
for (name in names(words.avoided)){
	stat = chisq.test(c(negative.differences[name,1],negative.differences[name,2]))$statistic
	words.avoided.stats = c(words.avoided.stats, stat)
}
names(words.avoided.stats) = names(words.avoided)
words.avoided.stats = sort(words.avoided.stats, decreasing=T)

# plotting functionality:
	if (visualization == "words"){
		words.preferred.by.primary.author = words.preferred.stats[words.preferred.stats>zeta.filter.threshold]
		words.avoided.by.primary.author = -(words.avoided.stats[words.avoided.stats>zeta.filter.threshold])
		
		preferred.words.for.plotting = c()
		preferred.indices.for.plotting = c()
		preferred.scores.for.plotting = c()
		for (i in 1:length(names(words.preferred.by.primary.author))){
			preferred.words.for.plotting = c(preferred.words.for.plotting, names(words.preferred.by.primary.author)[i])
			preferred.indices.for.plotting = c(preferred.indices.for.plotting, i)
			preferred.scores.for.plotting = c(preferred.scores.for.plotting, words.preferred.by.primary.author[[i]])	
		}
		
		avoided.words.for.plotting = c()
		avoided.indices.for.plotting = c()
		avoided.scores.for.plotting = c()
		for (i in 1:length(names(words.avoided.by.primary.author))){
			avoided.words.for.plotting = c(avoided.words.for.plotting, names(words.avoided.by.primary.author)[i])
			avoided.indices.for.plotting = c(avoided.indices.for.plotting, i)
			avoided.scores.for.plotting = c(avoided.scores.for.plotting, words.avoided.by.primary.author[[i]])	
		}
		
		plot.current.task = function(){
			plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(-100,100), type="n", xlab="Rank of the item", ylab="Score")
			text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7)
			text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7)
			abline(h=0, lty=2)	
			mtext("Preferred", side=4, at=50, las=3)
			mtext("Avoided", side=4, at=-50)
			title(main=graph.title)
		}
		
		if(titles.on.graph == TRUE) {
			graph.title = paste(basename(getwd()),"\n",long.method.name)
		} else {
			graph.title = ""}	 
		if(display.on.screen == TRUE){
			plot.current.task()
		}
		graph.filename <- paste(basename(getwd()),short.method.name, sep=" ")
		if(write.png.file == TRUE) {
			png(filename = paste(graph.filename,"%03d.png",sep=" "), 
				width=7,height=7,res=300, units="in")
			plot.current.task()
			dev.off()}
		if(write.pdf.file == TRUE) {
			pdf(file = paste(graph.filename,"%03d.pdf",sep=" "))
			plot.current.task()
			dev.off()}
	}
# end of plotting functionality
	
	
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
      file="words_preferred.txt", sep="\n")
# the current wordlist into a file
cat(words.preferred.by.primary.author, 
    file="words_preferred.txt", sep="\n",append=T)
#
#
# 


# some comments into the file containing worlist
cat("# The file contains words that were extracted in Burrows’ Zeta test:",
  "# this subset lists words significantly AVOIDED by primary author(s).",
  "# The list can be used as an input wordlist for other methods, and for this",
  "# purpose it can be manually revised, edited, deleted, culled, etc.", 
  "# You can either delete unwanted words, or mark them with \"#\"",
  "# -----------------------------------------------------------------------",
  "",
      file="words_avoided.txt", sep="\n")
# the current wordlist into a file
cat(words.avoided.by.primary.author, 
    file="words_avoided.txt", sep="\n",append=T)
#
#
# 
}
#} # <---- the first stage of the analysis is completed






### II stage ######################################################
if (stage.II.similarity.test == TRUE && oppose.method != "box.plot") {
# Note: it is impossible to perform stage II when the box.plot method is used

# checking if the test set exists and if it contains file(s)
	if(file.exists("test_set") == TRUE) {
		if (length(dir("test_set")) > 0) {

# retrieving the remaining names of samples
filenames.test.set = list.files("test_set")
#
#
# loading the test set from text files
corpus.of.test.set = list()
setwd("test_set")
  for (file in filenames.test.set) {
  # loading the next file from the list filenames.test.set,
  current.file = tolower(scan(file,what="char",sep="\n",quiet=T))
  # deleting punctuation, splitting into words:
  split.file = split.sample(current.file)  
  # appending the current text to the virtual corpus
  corpus.of.test.set[[file]] = split.file
  cat(file,"\t", "loaded successfully (",length(split.file)," words)\n",sep="")
  }
setwd("..")
#
# blank line on the screen
cat("\n")
#
}			
} else {
cat("No test set samples found\n",
    "Performing a simple comparison of the training samples...\n")
}
#
#
#

############################################################################
#############################################################################
#
# TODO: module for uploading existing lists of words preferred/avoided
# (similar thing is implemented in the STYLO script; it needs to be copied here)
#
##############################################################################
#############################################################################
#
#words.preferred.by.primary.author
#words.avoided.by.primary.author
###########################################X#################################


cat("\n\n\n\n")


# variable initiation
summary.zeta.scores = c()


# checking if the test set exists and if it contains file(s)
# depending on the ansewer, initializing 2 or 3 turns of the loop
if(file.exists("test_set") == TRUE) {
	if (length(dir("test_set")) > 0) {
loop.size = 3
}
} else {
loop.size = 2
}

# loop for (1) primary set and (2) secondary set
for(i in 1 : loop.size) {

  if(i == 1) {
  current.corpus = corpus.of.primary.set 
  filenames = filenames.primary.set  } 
  if(i == 2) {
  current.corpus = corpus.of.secondary.set 
  filenames = filenames.secondary.set }
  if(i == 3) {
  current.corpus = corpus.of.test.set 
  filenames = filenames.test.set }


# variable initiation
counted.occurrences = c()
current.corpus.scores = c()


# searching for words in texts slices
#
# loop for uploading each files
  for (file in filenames) {
  cat(file,"\n")
  file.basename = gsub("\\.txt$","",file)
    # loading the next sample from the list filenames.test.set,
    # deleting punctuation, splitting into words:
    current.sample = current.corpus[[file]]
    # the sample will be chopped into slices (variable preparation)
    text.length = length(current.sample)
    number.of.slices = floor(text.length/(text.slice.length-text.slice.overlap))
    # variable initiation
    table.of.occurrences = NULL

    # the current text is split into slices, the slices analyzed one by one
    for(new.slice in 1:number.of.slices) {
      start.point = new.slice * (text.slice.length-text.slice.overlap) -
                                   (text.slice.length-text.slice.overlap) +1
      current.slice = current.sample[start.point:(start.point+text.slice.length-1)]
      # check the wordlist against the current slice
      yyy = as.numeric(words.preferred.by.primary.author %in% current.slice)
      xxx = as.numeric(words.avoided.by.primary.author %in% current.slice)
      word.occurrence.count = c(sum(xxx)/length(xxx)*100,
                               (sum(yyy)/length(yyy)*100) )
      table.of.occurrences = rbind(table.of.occurrences,word.occurrence.count)
      }

rownames(table.of.occurrences) = paste(file.basename,1:new.slice,sep="_")
current.corpus.scores = rbind(current.corpus.scores,table.of.occurrences)

}   # <--- loop for uploading texts in a given corpus


# building a table of final results, with an appropriate class name assigned
current.corpus.scores = cbind(current.corpus.scores,
                        rep(c("primary","secondary","unknown")[i],
                        length(current.corpus.scores[,1])))
summary.zeta.scores = rbind(summary.zeta.scores,current.corpus.scores)
#summary.zeta.scores = as.data.frame(summary.zeta.scores)
colnames(summary.zeta.scores) = c("preferred","avoided","class")
}


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
  plot.current.task = function() {NULL}
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
    graph.title = paste(basename(getwd()),"\n",long.method.name)
    } else {
    graph.title = ""}	 

    

  if(display.on.screen == TRUE){
  plot.current.task()
  }
  graph.filename <- paste(basename(getwd()),short.method.name, sep=" ")
  if(write.png.file == TRUE) {
  png(filename = paste(graph.filename,"%03d.png",sep=" "), 
           width=7,height=7,res=300, units="in")
  plot.current.task()
  dev.off()}
  if(write.pdf.file == TRUE) {
  pdf(file = paste(graph.filename,"%03d.pdf",sep=" "))
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
if(exists("classification.results")) {
  attr(classification.results, "description") = "so far, there's nothing here"
}
if(exists("variable.to.be.done")) {
  attr(variable.to.be.done, "description") = "so far, there's nothing here"
}
if(exists("yet.another.variable")) {
  attr(yet.another.variable, "description") = "another empty variable"
}






# Creating an object (list) that will contain the final results,
# including the preferred and avoided words
# This list will be turned into the class "stylo.results"
results.oppose = list()
# elements that we want to add on this list
variables.to.save = c("words.preferred",
                      "words.avoided",
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
