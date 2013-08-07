

##############################################################################
# this is simply the Oppose script put into function(){ }

# it needs to be thoroughly re-written


oppose <-
function(gui=TRUE,path="",
         primary.corpus.dir="primary_set",
         secondary.corpus.dir="secondary_set",
         test.corpus.dir="test_set") {



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







stage.I.word.selection = TRUE
stage.II.similarity.test = TRUE

text.slice.length = 2000
text.slice.overlap = 0

# if you want to ignore words that occurred only once or twice, set
# the threshold to 2; if you want to analyze all the words, set 0
rare.occurrences.threshold = 2


# choose one: "craig.zeta" | "eder.zeta" | "chisquare.zeta" | "mann.whitney" | "box.plot"
method = "craig.zeta"

### the meaning of the threshold parameter varies with the method chosen: #####
# "craig.zeta" method chosen, you might probably want to filter out some words
# of weak discrimination strength. Provided that 2 means the strongest 
# positive difference and O the strongest negative difference (Hoover 2009), 
# the values either just above or just below 1 are not significant and
# thus can be (or rather should be) omitted. If chisquare method was chosen,
# all the differences of p-value below 0.05 were filtered out, in pure Zeta,
# there is no a priori solution. Threshold 0.2 would filter out a vast majority
# of words, threshold set to 1 would filter all the words in a corpus.
threshold = 0.2

# initialize the token to be plotted if the box.plot method is chosen
plot.token = ""


graphic.output = TRUE
display.on.screen = TRUE
write.pdf.file = FALSE
write.png.file = FALSE
use.color.graphs = TRUE
titles.on.graph = TRUE
polygons.on.graph = FALSE
identify.points = FALSE
classification = FALSE
naive.bayes = FALSE
svm.classification = TRUE
decision.tree.classification = FALSE
visualization = "words"

# #################################################
#
# the GUI module 
#
# #################################################

# At the beginning of the script, you could decide whether use tp the GUI module 
# or not; if the appropriate option was switched on, the GUI will start now

if (gui == TRUE) {
  #library(tcltk2)

if(file.exists("oppose_config.txt") == TRUE) {
	source("oppose_config.txt") }

  cancel_pause <- FALSE
  tt <- tktoplevel()
  tktitle(tt) <- "oppose script | set parameters"

  push_OK <- function(){
      cancel_pause <<- TRUE
      tkdestroy(tt)
      }

text.slice.length <- tclVar(text.slice.length)
text.slice.overlap <- tclVar(text.slice.overlap)
rare.occurrences.threshold <- tclVar(rare.occurrences.threshold)
threshold <- tclVar(threshold)
plot.token <- tclVar(plot.token)
method <- tclVar(method)
display.on.screen <- tclVar(display.on.screen)
write.pdf.file <- tclVar(write.pdf.file)
write.png.file <- tclVar(write.png.file)
use.color.graphs <- tclVar(use.color.graphs)
titles.on.graph <- tclVar(titles.on.graph)
identify.points <- tclVar(identify.points)
classification <-tclVar(classification)
	

visualization <- tclVar(visualization)

entry_none <- tkradiobutton(tt)
entry_words <- tkradiobutton(tt)
entry_markers <- tkradiobutton(tt)

tkconfigure(entry_none,variable=visualization,value="none")
tkconfigure(entry_words,variable=visualization,value="words")
tkconfigure(entry_markers,variable=visualization,value="markers")

entrylabel_none <- tklabel(tt,text="None",anchor="w")
entrylabel_words <- tklabel(tt,text="Words",anchor="w")
entrylabel_markers <- tklabel(tt,text="Markers",anchor="w")

cb_display.on.screen <- tkcheckbutton(tt)
cb_write.pdf.file <- tkcheckbutton(tt)
cb_write.png.file <- tkcheckbutton(tt)
cb_use.color.graphs <- tkcheckbutton(tt)
cb_titles.on.graph <- tkcheckbutton(tt)
cb_identify.points <- tkcheckbutton(tt)
cb_classification <- tkcheckbutton(tt)

rb_craig.zeta <- tkradiobutton(tt)
rb_eder.zeta <- tkradiobutton(tt)
rb_chisquare.zeta <- tkradiobutton(tt)
rb_mann.whitney <- tkradiobutton(tt)
rb_box.plot <- tkradiobutton(tt)

tt_text.slice.length <- tkentry(tt,textvariable=text.slice.length,width="8")
tt_text.slice.overlap <- tkentry(tt,textvariable=text.slice.overlap,width="8")
tt_rare.occurences.threshold <- tkentry(tt,textvariable=rare.occurrences.threshold,width="8")
tt_threshold <- tkentry(tt,textvariable=threshold,width="8")
tt_plot.token <-tkentry(tt,textvariable=plot.token,width="16")

button_1 <- tkbutton(tt,text="     OK     ",command=push_OK,relief="groove")
tkbind(button_1,"<Return>",push_OK) 

tkconfigure(cb_display.on.screen,variable=display.on.screen)
tkconfigure(cb_write.pdf.file,variable=write.pdf.file)
tkconfigure(cb_write.png.file,variable=write.png.file)
tkconfigure(cb_use.color.graphs,variable=use.color.graphs)
tkconfigure(cb_titles.on.graph,variable=titles.on.graph)
tkconfigure(cb_identify.points,variable=identify.points)
tkconfigure(cb_classification,variable=classification)
	


tkconfigure(rb_craig.zeta,variable=method,value="craig.zeta")
tkconfigure(rb_eder.zeta,variable=method,value="eder.zeta")
tkconfigure(rb_chisquare.zeta,variable=method,value="chisquare.zeta")
tkconfigure(rb_mann.whitney,variable=method,value="mann.whitney")
tkconfigure(rb_box.plot,variable=method,value="box.plot")


rblab_craig.zeta <- tklabel(tt,text="Craig's Zeta   ",anchor="w")
rblab_eder.zeta <- tklabel(tt,text="Eder's Zeta    ",anchor="w")
rblab_chisquare.zeta <- tklabel(tt,text="Chi-square Zeta          ",anchor="w")
rblab_mann.whitney <- tklabel(tt,text="Mann-Whitney          ",anchor="w")
rblab_box.plot <- tklabel(tt,text="Boxplot          ",anchor="w")

ttlab_text.slice.length <- tklabel(tt,text="Slice Length     ")
ttlab_text.slice.overlap <- tklabel(tt,text="Slice Overlap     ")
ttlab_rare.occurences.threshold <- tklabel(tt,text="Occurrence Threshold    ")
ttlab_threshold <- tklabel(tt,text="Filter Threshold    ")
ttlab_plot.token <- tklabel(tt,text="Plot token    ")

tkgrid(tklabel(tt,text="    ")) # blank line
	

tkgrid(tklabel(tt,text="            INPUT:               "), ttlab_text.slice.length,ttlab_text.slice.overlap, sticky="w")
tkgrid(tklabel(tt,text="                                 "), tt_text.slice.length, tt_text.slice.overlap, sticky="w")
tkgrid(tklabel(tt,text="    ")) # blank line
tkgrid(tklabel(tt,text="                                 "), ttlab_rare.occurences.threshold, ttlab_threshold, sticky="w")
tkgrid(tklabel(tt,text="                                 "), tt_rare.occurences.threshold, tt_threshold, sticky="w")


tkgrid(tklabel(tt,text="    ")) # blank line

tkgrid(tklabel(tt,text="            METHOD:     "), rblab_craig.zeta, rblab_eder.zeta, rblab_chisquare.zeta, sticky="w")
tkgrid(tklabel(tt,text="                   "), rb_craig.zeta, rb_eder.zeta, rb_chisquare.zeta, sticky="w")
tkgrid(tklabel(tt,text="                   "), rblab_mann.whitney, rblab_box.plot, sticky="w")
tkgrid(tklabel(tt,text="                   "), rb_mann.whitney, rb_box.plot, sticky="w")	


tkgrid(tklabel(tt,text="    ")) # blank line

cblab_display.on.screen <- tklabel(tt,text="Onscreen")
cblab_write.pdf.file <- tklabel(tt,text="PDF")
cblab_write.png.file <- tklabel(tt,text="PNG")
cblab_use.color.graph <- tklabel(tt,text="Colors")
cblab_titles.on.graph <- tklabel(tt,text="Titles")
cblab_identify.points <- tklabel(tt,text="Identify Points")
cblab_classification <- tklabel(tt, text="Classification")
	

tkgrid(tklabel(tt,text="  VISUALIZATION:           "),entrylabel_none,entrylabel_words,entrylabel_markers,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_none,entry_words,entry_markers,sticky="w")

tkgrid(tklabel(tt,text="  MISCELLANEOUS:           "), cblab_display.on.screen, cblab_write.pdf.file, cblab_write.png.file, sticky="w")
tkgrid(tklabel(tt,text="                   "), cb_display.on.screen, cb_write.pdf.file, cb_write.png.file, sticky="w")
tkgrid(tklabel(tt,text="                   "), cblab_use.color.graph, cblab_titles.on.graph ,cblab_identify.points, sticky="w")
tkgrid(tklabel(tt,text="                   "), cb_use.color.graphs, cb_titles.on.graph ,cb_identify.points, sticky="w")
tkgrid(tklabel(tt,text="                   "), cblab_classification, ttlab_plot.token, sticky="w")
tkgrid(tklabel(tt,text="                   "), cb_classification, tt_plot.token, sticky="w")

tkgrid(tklabel(tt,text="    ")) # blank line

tkgrid(button_1,columnspan="10")

tkgrid(tklabel(tt,text="    ")) # blank line

repeat{
  if(cancel_pause){
    text.slice.length <- as.numeric(tclvalue(text.slice.length))
    text.slice.overlap <- as.numeric(tclvalue(text.slice.overlap))
    rare.occurrences.threshold <- as.numeric(tclvalue(rare.occurrences.threshold))
    threshold <- as.numeric(tclvalue(threshold))
    display.on.screen <- as.logical(as.numeric(tclvalue(display.on.screen)))
    write.pdf.file <- as.logical(as.numeric(tclvalue(write.pdf.file)))
    write.png.file <- as.logical(as.numeric(tclvalue(write.png.file)))
    use.color.graphs <- as.logical(as.numeric(tclvalue(use.color.graphs)))
    titles.on.graph <- as.logical(as.numeric(tclvalue(titles.on.graph)))
    identify.points <- as.logical(as.numeric(tclvalue(identify.points)))
	visualization <- as.character(tclvalue(visualization))
	classification <- as.logical(as.numeric(tclvalue(classification)))
    method <- as.character(tclvalue(method))
	plot.token <- as.character(tclvalue(plot.token))
    break
  }
}

} # <-- here the option "interactive.mode.with.GUI == TRUE" is completed

      
# #################################################
# GUI module explicit feliciter (Phew!)
# #################################################


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
  ### Linux, Mac
  tokenized.sample = c(unlist(strsplit(input.text, "[^[:alpha:]]+")))
    if(Sys.info()[["sysname"]] == "Windows") { 
	### Windows
	tokenized.text = c(unlist(strsplit(input.text, "\\W+|_+",perl=T)))
	} else {
	### Linux, Mac
	tokenized.text = c(unlist(strsplit(input.text, "[^[:alpha:]]+")))
	}
  tokenized.sample = tokenized.sample[grep("[^[:digit:]]",tokenized.sample)]
}



###############################################################
# Function for drawing polygons around clouds of points of I and II set: 
# a module as rough as can be (this is a pre-alpha version!)
# Argument: a data frame containing 3 columns: x, y, class
###############################################################
draw.polygons = function(summary.zeta.scores) {
  for(current.subset in c("primary","secondary")) {
    # extracting the points from the I/II set
    current.cloud = (summary.zeta.scores[grep(current.subset,summary.zeta.scores[,3]),1:2])
    current.cloud = as.numeric(as.matrix(current.cloud))
    current.cloud = matrix(current.cloud,ncol=2)
    current.cloud = as.data.frame(current.cloud)
    colnames(current.cloud) = c("x","y")
    # extreme right point
    a1 = current.cloud[current.cloud[,1] == max(current.cloud[,1]),]
    # if there are more points of the same $x, then select max(a2$y)
    a1 = a1[a1$y == max(a1$y),]
    # just in case (theoretically, there still might be more that one point)
    a1 = a1[1,]
    # extreme left point
    a2 = current.cloud[current.cloud[,1] == min(current.cloud[,1]),]
    # if there are more points of the same $x, then select min(a2$y)
    a2 = a2[a2$y == min(a2$y),]
    # just in case (theoretically, there still might be more that one point)
    a2 = a2[1,]
    #
    #
    a.i = a1
    a.j = a2
    #
    # variable initialization
    vertexes.of.polygon = a1
    #
    #
    for(o in 1: length(current.cloud[,1]) ) {
      for(m in 1: length(current.cloud[,1]) ) {
        does.the.point.fit = a.j
        # equation for a line through (any) two given points
        # just in case: for drawing vertical lines
        if(a.i$x == a.j$x) {a.i$x = a.i$x + a.i$x *0.01}
        # steepnes with a safety parameter (to avoid dividing by 0) 
        steepness = (a.i$y-a.j$y)/((a.i$x-a.j$x) + 0.000001)
        offset = a.j$y - (steepness * a.j$x)
        current.line = steepness + offset
        #
        # fitting any better line
        for(n in 1: length(current.cloud[,1]) ) {
          current.point = current.cloud[n,]
          if(current.point$x * steepness + offset > 
                            current.point$y + current.point$y * .01) { 
            a.j = current.point
            break
            }
        } # <-- loop n
        #
        if(a.j$y == does.the.point.fit$y && a.j$x == does.the.point.fit$x) { 
          break
          }
      } # <-- loop m
      #
      vertexes.of.polygon = rbind(vertexes.of.polygon, a.j)
      #
      if(a.j$y == a2$y && a.j$x == a2$x) { 
        break
        }
      #
      # current a.i (that turned to be the optimal point) becomes a new vertex
      a.i = a.j
      # consequently, the end point should be reset
      a.j = a2
    } # <-- loop o
    #
    #### the same procedure applied to the II set
    #
    a.i = a2
    a.j = a1
    #
    for(o in 1: length(current.cloud[,1]) ) {
      for(m in 1: length(current.cloud[,1]) ) {
        does.the.point.fit = a.j
        # equation for a line through (any) two given points
        # just in case: for drawing vertical lines
        if(a.i$x == a.j$x) {a.i$x = a.i$x - a.i$x *0.01}
        # steepnes with a safety parameter (to avoid dividing by 0)
        steepness = (a.i$y-a.j$y)/((a.i$x-a.j$x) + 0.000001)
        offset = a.j$y - (steepness * a.j$x)
        current.line = steepness + offset
        #
        # fitting any better line
        for(n in 1: length(current.cloud[,1]) ) {
          current.point = current.cloud[n,]
          if(current.point$x * steepness + offset < 
                            current.point$y - current.point$y * .01) { 
            a.j = current.point
            break
            }
        } # <-- loop n
        #
        if(a.j$y == does.the.point.fit$y && a.j$x == does.the.point.fit$x) { 
          break
          }
      } # <-- loop m
      #
      vertexes.of.polygon = rbind(vertexes.of.polygon, a.j)
      #
      if(a.j$y == a1$y && a.j$x == a1$x) { 
        break
        }
      #
      # current a.i (that turned to be the optimal point) becomes a new vertex
      a.i = a.j
      # consequently, the end point should be reset
      a.j = a1
    } # <-- loop o
  #
  #
  # final variables’ assignments
  if(current.subset == "primary") {
    polygon.primary = vertexes.of.polygon
    } else {
    polygon.secondary = vertexes.of.polygon
    }
  } # <-- loop current.subset
  #
  # drawing two polygons
  polygon(polygon.primary,col=rgb(0,0,0,0.1),border=2)
  polygon(polygon.secondary,col=rgb(0,0,0,0.1),border=3)
}



############################################################################
############################################################################







############################################################################
#
# retrieving the names of samples
filenames.primary.set = list.files(primary.corpus.dir)
filenames.secondary.set = list.files(secondary.corpus.dir)
#
#
# loading the primary set from text files
corpus.of.primary.set = list()
setwd(primary.corpus.dir)
  for (file in filenames.primary.set) {
  # loading the next file from the list filenames.primary.set,
  current.file = tolower(scan(file,what="char",sep="\n",quiet=T))
  # deleting punctuation, splitting into words:
  split.file = split.sample(current.file)
  # appending the current text to the virtual corpus
  corpus.of.primary.set[[file]] = split.file
    cat(file,"\t", "loaded successfully (",length(split.file)," words)\n",sep="")
    if(length(split.file) < text.slice.length) {
    error.message = TRUE; setwd("..")
    cat("the above file is too short for being split into", 
         text.slice.length,"words\n")
    stop("change your settings!")
    }
  }
setwd("..")

# loading the secondary set from text files
corpus.of.secondary.set = list()
setwd(secondary.corpus.dir)
  for (file in filenames.secondary.set) {
  # loading the next file from the list filenames.secondary.set,
  current.file = tolower(scan(file,what="char",sep="\n",quiet=T))
  # deleting punctuation, splitting into words:
  split.file = split.sample(current.file)
  # appending the current text to the virtual corpus
  corpus.of.secondary.set[[file]] = split.file
  cat(file,"\t", "loaded successfully (",length(split.file)," words)\n",sep="")
    if(length(split.file) < text.slice.length) {
    error.message = TRUE; setwd("..")
    cat("the above file is too short for being split into", 
         text.slice.length,"words\n")
    stop("change your settings!")
    }
  }
setwd("..")
# blank line on the screen
cat("\n")


### I stage ######################################################

# the first stage of zeta test (might be skipped if you already have 
# a list of distinctive words)
if(stage.I.word.selection == TRUE) {

# We need a list of the most frequent words used in the current corpus, 
# in descendent order, without frequencies (just a list of words). 
wordlist.of.primary.set = c()
	for (file in 1 : length(corpus.of.primary.set)) {
		# loading the next sample from the list filenames.primary.set
		current.text = corpus.of.primary.set[[file]]
		# putting samples together:
		wordlist.of.primary.set = c(wordlist.of.primary.set, current.text)
		cat(names(corpus.of.primary.set[file]),"\t","tokenized successfully", "\n")
	}
wordlist.of.secondary.set = c()
	for (file in 1 : length(corpus.of.secondary.set)) {
		# loading the next sample from the list filenames.secondary.set
		current.text = corpus.of.secondary.set[[file]]
		# putting samples together:
		wordlist.of.primary.set = c(wordlist.of.secondary.set, current.text)
		cat(names(corpus.of.secondary.set[file]),"\t","tokenized successfully","\n")
		#wordlist.of.secondary.set = c()
	}

cat("\n\nThinking...\n\n")

# preparing a sorted frequency list of the whole corpus
mfw.list.of.all = sort(table(c(wordlist.of.primary.set,wordlist.of.secondary.set)), decreasing=T)
# we want to filter out rare words (e.g. hapax legomena and/or dislegomena)
mfw.list.of.all = mfw.list.of.all[mfw.list.of.all > rare.occurrences.threshold]

# the only thing we need are words ordered by frequency (no frequencies)
wordlist = names(mfw.list.of.all)

# ###############################################################

cat("\nExtracting distinctive words... (this might take a while)\n\n")

primary.slices = c()
secondary.slices = c()	

# loop for (1) primary set and (2) secondary set
for(i in 1:2) {
	if(i == 1) {
		current.corpus = corpus.of.primary.set 
		filenames = filenames.primary.set
	} else {
		current.corpus = corpus.of.secondary.set 
		filenames = filenames.secondary.set
	}
	# variable initiation
	counted.occurrences = c()

  # loop for uploading each file
  for (file in filenames) {
  cat(file,"\n")
    # loading the next sample from the list filenames.primary.set
    # deleting punctuation, splitting into words:
    current.sample = current.corpus[[file]]
    # the sample will be chopped into slices (variable preparation)
    text.length = length(current.sample)
    number.of.slices = floor(text.length/(text.slice.length-text.slice.overlap))
    number.of.slices = floor(text.length/text.slice.length)
    # variable initiation
    table.of.occurrences = NULL

	# the current text is split into slices, the slices analyzed one by one
	for(new.slice in 1:number.of.slices) {
		start.point = new.slice * (text.slice.length-text.slice.overlap) -
                              (text.slice.length-text.slice.overlap) +1
		current.slice = current.sample[start.point:(start.point+text.slice.length-1)]
		# check the wordlist against the current slice
		word.occurrence.count = as.numeric(wordlist %in% current.slice)
		table.of.occurrences = rbind(table.of.occurrences,word.occurrence.count)
		# save slice for future use:
		if(i == 1){
			primary.slices[[paste("prim", length(primary.slices), sep="")]] = current.slice
		} else {
			secondary.slices[[paste("sec", length(secondary.slices), sep="")]] = current.slice
		}
	}
	# adding the slices, extracting the list of words analyzed
	no.of.successful.slices = colSums(table.of.occurrences)/number.of.slices*100
	names(no.of.successful.slices) = wordlist
	counted.occurrences = cbind(counted.occurrences, no.of.successful.slices)
}   # <--- loop for uploading next tests in a given corpus

# if more texts were analyzed, calculate the arithmetic mean
counted.occurrences = colMeans(as.data.frame(t(counted.occurrences)))

if(i == 1) {
	comparison.primary = counted.occurrences
  } else {
	comparison.secondary = counted.occurrences
  }

}  # <-- loop uploading the current corpus returns here





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
	var.name(threshold)
	var.name(method)
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
if (method == "box.plot"){
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
if (method == "mann.whitney"){
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
	tail = round(length(statistics)/100*threshold)
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

if(method == "craig.zeta") {
long.method.name="Craig's Zeta"
short.method.name="Craig"
# two ways of expressing the same thing:
differences = ( comparison.primary - comparison.secondary ) / 100 + 1 # note: add-one smoothing
#differences = (comparison.primary + (100 - comparison.secondary )) / 100

# plotting functionality:
if (visualization == "words" && method != "box.plot"){
words.preferred.by.primary.author = sort(differences[differences > 1 + threshold],decreasing=TRUE)
words.avoided.by.primary.author = sort(differences[differences < 1 - threshold])

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
												   + threshold],decreasing=TRUE))
words.avoided.by.primary.author = names(sort(differences[differences < 1 
												 - threshold]))
}


# Eder's zeta (inspired by Canberra distance measure)
#####################################################################

if(method == "eder.zeta") {
long.method.name="Eder's Zeta"
short.method.name="Eder"
# computation of the differences between occurences of words
differences = ( comparison.primary - comparison.secondary ) / ( comparison.primary + comparison.secondary )
differences = differences[differences > -1]
differences = differences[differences < 1]

# extracting the distinctive words
words.preferred.by.primary.author = names(sort(differences[differences > 0 + threshold], decreasing=TRUE))
words.avoided.by.primary.author = names(sort(differences[differences < 0 - threshold]))

# plotting functionality:
	if (visualization == "words"){
		
		words.preferred.by.primary.author = sort(differences[differences > 0 + threshold],decreasing=TRUE)
		words.avoided.by.primary.author = sort(differences[differences < 0 - threshold])
		
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
words.preferred.by.primary.author = names(sort(differences[differences > 1 + threshold],decreasing=TRUE))
words.avoided.by.primary.author = names(sort(differences[differences < 1 - threshold]))
}

# Zeta based on chi-square test
#####################################################################

if(method == "chisquare.zeta") {
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
		words.preferred.by.primary.author = words.preferred.stats[words.preferred.stats>threshold]
		words.avoided.by.primary.author = -(words.avoided.stats[words.avoided.stats>threshold])
		
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

if (method != "box.plot"){
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
} # <---- the first stage of the analysis is completed






### II stage ######################################################
if (stage.II.similarity.test == TRUE && method != "box.plot") {
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
if ((visualization == "markers") && (method != "box.plot")){
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

if (classification == TRUE && method != "box.plot") {
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






# creating an object (list) that will contain the final results,
# tables of frequencies, etc.etc.
results.oppose = list()
# elements that we want to add on this list
variables.to.save = c("words.preferred.by.primary.author",
                      "words.avoided.by.primary.author",
                      "classification.results")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.oppose[[i]] = get(i)
}




cat("\n")
cat("Some results should have been written into a few files; you should\n")
cat("be able to find them in your current (working) directory. These include\n")
cat("a list of words significantly preferred in the primary set,\n")
cat("words significantly avoided (or, preferred in the secondary set), etc.\n")
cat("Advanced users: you can pipe the results to a variable, e.g.:\n")
cat("    my.results = oppose()\n")
cat("this will create a list containing some presumably interesting stuff.\n")
cat("The list created, you can type, e.g.:\n")
cat("    summary(my.results)\n")
cat("to see which variables are stored there.\n")
cat("\n")
cat("\n")
cat("for suggestions how to cite this software, type: citation(\"stylo\")\n")
cat("\n")
cat("\n")




# return (tacitly) the value of the function 
invisible(results.oppose)
}
