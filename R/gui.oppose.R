

# #################################################
# Function for displaying simple yet effective graphical interface (GUI).
# If you execute this option, the values stored in the function 
# stylo.default.settings() will serve as default for the GUI for the first run.
# In the subsequent runs, recent values will appear as default in the GUI.
# No optional arguments.
# Usage: gui.oppose()
# #################################################    

gui.oppose <-
function(...) {
  
  # loading required library  #### it will not be needed
  #  library(tcltk2)





  # using recently used settings to overwrite the default options
  restored.variables = list()
  if(file.exists("oppose_config.txt") == TRUE) {
    source("oppose_config.txt", local = TRUE) 
    # adding them on a list
    filtered.variables = ls()[!ls() %in% c("restored.variables","filtered.variables")]
    for(i in filtered.variables) {
      restored.variables[[i]] = get(i)
      }
    }



  # loading default settings
  default.variables = stylo.default.settings(...)

  # if any command-line arguments have been passed by a user, they will
  # be stored on the following list and used to overwrite the defaults
  passed.arguments = list(...)
  


    
# Code that enables overwriting the variables with custom settings.
# A magnificent snipped for combining two lists 
# http://stackoverflow.com/questions/13811501/r-merge-lists-with-overwrite-and-recursion
merge.lists <- function(a, b) {
    a.names <- names(a)
    b.names <- names(b)
    m.names <- sort(unique(c(a.names, b.names)))
    sapply(m.names, function(i) {
        if (is.list(a[[i]]) & is.list(b[[i]])) merge.lists(a[[i]], b[[i]])
        else if (i %in% b.names) b[[i]]
        else a[[i]]
    }, simplify = FALSE)
}

# if any variables have been passed as arguments, they will overwrite
# the default settings
variables.tmp = merge.lists(default.variables,restored.variables)
variables = merge.lists(variables.tmp, passed.arguments)





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
encoding = variables$encoding
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


number.of.samples = variables$number.of.samples


text.slice.length = variables$text.slice.length
text.slice.overlap = variables$text.slice.overlap
rare.occurrences.threshold = variables$rare.occurrences.threshold
oppose.method = variables$oppose.method
zeta.filter.threshold = variables$zeta.filter.threshold
use.color.graphs = variables$use.color.graphs
polygons.on.graph = variables$polygons.on.graph
identify.points = variables$identify.points
classification = variables$classification
naive.bayes = variables$naive.bayes
svm.classification = variables$svm.classification
decision.tree.classification = variables$decision.tree.classification
visualization = variables$visualization
plot.token = variables$plot.token




# a not very elegant way of switching from strings into logical values
encoding.orig = variables$encoding
  if(variables$encoding == "UTF-8") {
    encoding = TRUE
  } else {
    encoding = FALSE
  }



  
  .Tcl("font create myDefaultFont -family tahoma -size 8")
  .Tcl("option add *font myDefaultFont")  
  
    tt <- tktoplevel()
    tktitle(tt) <- "oppose | set parameters"
    
    push_OK <- function(){
        tkdestroy(tt)
        }



text.slice.length <- tclVar(text.slice.length)
text.slice.overlap <- tclVar(text.slice.overlap)
rare.occurrences.threshold <- tclVar(rare.occurrences.threshold)
zeta.filter.threshold <- tclVar(zeta.filter.threshold)
plot.token <- tclVar(plot.token)
oppose.method <- tclVar(oppose.method)
display.on.screen <- tclVar(display.on.screen)
write.pdf.file <- tclVar(write.pdf.file)
write.png.file <- tclVar(write.png.file)
use.color.graphs <- tclVar(use.color.graphs)
titles.on.graphs <- tclVar(titles.on.graphs)
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
tt_zeta.filter.threshold <- tkentry(tt,textvariable=zeta.filter.threshold,width="8")
tt_plot.token <-tkentry(tt,textvariable=plot.token,width="16")

button_1 <- tkbutton(tt,text="     OK     ",command=push_OK,relief="groove")
tkbind(button_1,"<Return>",push_OK) 

tkconfigure(cb_display.on.screen,variable=display.on.screen)
tkconfigure(cb_write.pdf.file,variable=write.pdf.file)
tkconfigure(cb_write.png.file,variable=write.png.file)
tkconfigure(cb_use.color.graphs,variable=use.color.graphs)
tkconfigure(cb_titles.on.graph,variable=titles.on.graphs)
tkconfigure(cb_identify.points,variable=identify.points)
tkconfigure(cb_classification,variable=classification)
	


tkconfigure(rb_craig.zeta,variable=oppose.method,value="craig.zeta")
tkconfigure(rb_eder.zeta,variable=oppose.method,value="eder.zeta")
tkconfigure(rb_chisquare.zeta,variable=oppose.method,value="chisquare.zeta")
tkconfigure(rb_mann.whitney,variable=oppose.method,value="mann.whitney")
tkconfigure(rb_box.plot,variable=oppose.method,value="box.plot")


rblab_craig.zeta <- tklabel(tt,text="Craig's Zeta   ",anchor="w")
rblab_eder.zeta <- tklabel(tt,text="Eder's Zeta    ",anchor="w")
rblab_chisquare.zeta <- tklabel(tt,text="Chi-square Zeta          ",anchor="w")
rblab_mann.whitney <- tklabel(tt,text="Mann-Whitney          ",anchor="w")
rblab_box.plot <- tklabel(tt,text="Boxplot          ",anchor="w")

ttlab_text.slice.length <- tklabel(tt,text="Slice Length     ")
ttlab_text.slice.overlap <- tklabel(tt,text="Slice Overlap     ")
ttlab_rare.occurences.threshold <- tklabel(tt,text="Occurrence Threshold    ")
ttlab_zeta.filter.threshold <- tklabel(tt,text="Filter Threshold    ")
ttlab_plot.token <- tklabel(tt,text="Plot token    ")

tkgrid(tklabel(tt,text="    ")) # blank line
	

tkgrid(tklabel(tt,text="            INPUT:               "), ttlab_text.slice.length,ttlab_text.slice.overlap, sticky="w")
tkgrid(tklabel(tt,text="                                 "), tt_text.slice.length, tt_text.slice.overlap, sticky="w")
tkgrid(tklabel(tt,text="    ")) # blank line
tkgrid(tklabel(tt,text="                                 "), ttlab_rare.occurences.threshold, ttlab_zeta.filter.threshold, sticky="w")
tkgrid(tklabel(tt,text="                                 "), tt_rare.occurences.threshold, tt_zeta.filter.threshold, sticky="w")


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


  # wait until we have input
  tkwait.window(tt)

    variables$text.slice.length <- as.numeric(tclvalue(text.slice.length))
    variables$text.slice.overlap <- as.numeric(tclvalue(text.slice.overlap))
    variables$rare.occurrences.threshold <- as.numeric(tclvalue(rare.occurrences.threshold))
    variables$zeta.filter.threshold <- as.numeric(tclvalue(zeta.filter.threshold))
    variables$display.on.screen <- as.logical(as.numeric(tclvalue(display.on.screen)))
    variables$write.pdf.file <- as.logical(as.numeric(tclvalue(write.pdf.file)))
    variables$write.png.file <- as.logical(as.numeric(tclvalue(write.png.file)))
    variables$use.color.graphs <- as.logical(as.numeric(tclvalue(use.color.graphs)))
    variables$titles.on.graphs <- as.logical(as.numeric(tclvalue(titles.on.graphs)))
    variables$identify.points <- as.logical(as.numeric(tclvalue(identify.points)))
    variables$visualization <- as.character(tclvalue(visualization))
    variables$classification <- as.logical(as.numeric(tclvalue(classification)))
    variables$oppose.method <- as.character(tclvalue(oppose.method))
    variables$plot.token <- as.character(tclvalue(plot.token))

  .Tcl("font delete myDefaultFont")

# when encoding goes into GUI, this statement will have to be deleted
variables$encoding = encoding  

  # switching back from logical values into strings
  if(variables$encoding == TRUE) {
    variables$encoding = "UTF-8"
  } else {
    variables$encoding = "native.enc"
  }



return(variables)
}
