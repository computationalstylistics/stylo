

##############################################################################
# this is simply the Classify script put into function(){ }

# it needs to be thoroughly re-written:
# (1) gui should be extracted as a separated function
# (2) it will be better to have initial settings stored in a separate function,
#     provided that this is a good idea (cf. stylo() and stylo.default.settings())
# (3) uploading corpora: the function load.corpus.and.parse should be used


classify <-
function(gui = TRUE, path = "",
         training.corpus.dir = "primary_set",
         test.corpus.dir = "secondary_set") {

# so far, 'path', 'training.corpus' and 'test.corpus' are not used
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




#######  GENERAL SETTINGS (GUI/TEXT-MODE)  ###################################

# If you wish to use a simple yet effective graphical interface (GUI),
# just set the following option to TRUE, otherwise switch this option to FALSE
# and edit manually the rest of variables (see below).
# If you switch this option on, the values indicated in the following sections 
# will serve as default for the GUI (you can adapt them to your needs)

interactive.mode.with.GUI = TRUE


#######  TEXT- AND LANGUAGE-DEPENDENT SETTINGS  ####################


# format of corpus files; available choices are:
# "plain", "xml", "xml.drama", "xml.notitles", "html"
corpus.format = "plain"


# how many MFW should be taken into analysis (if mfw.min value = max.mfw, 
# then no multiple iterations will be computed)
# start.at option enables skipping top frequency words: you should
# indicate the desired start position of your list (in most cases you will 
# probably prefer setting it to 1)

mfw.min = 100
mfw.max = 100
mfw.incr = 100
start.at = 1

# culling rate specifies the number of texts in a corpus in which a given word 
# must be found in order to be included in the analysis. Thus, a 100% culling 
# rate limits the analysis to words that appear at least once in every text 
# in the corpus; at a 50% culling rate, a word is included into the analysis 
# when it appears in at least half of the texts in the corpus; a 0% culling 
# rate (or no culling) means that no words are omitted.
# about min=max: see above

culling.min = 0
culling.max = 0
culling.incr = 20

# Deleting pronouns (this is independent of the culling procedure).
# If deleting pronouns option is switched to TRUE, choose one language
# of the following: English, Polish, Latin, French, German, Italian, Hungarian
# (the editable lists of pronouns are available below; see: advanced settings).
# Additionally, there are a few variants of language settings available:
# English.contr, English.all, and Latin.corr. Their meaning is as follows:
#     "English.contr": treats the contractions as signle words, i.e. strings
#         such as "don't", "you've" etc. will not be split into two words.
#     "English.all": keeps the contractions (as above), and also prevents
#         from splitting compound words (mother-in-law, double-decker, etc.)
#     "Latin.corr": since some editions do not distinguish the letters v/u,
#         this option provides a consistent conversion to "u" in each text.

delete.pronouns = FALSE
corpus.lang = "English.all"


# Selection of features. In classical approaches, frequencies of the most
# frequent words (MFW) are used as basis for multidimensional analyses.
# It has been argued, however, that also other features are worh considering,
# especially word and/or letter n-grams. The general concept of n-gram
# is to combine a string of single words/letters into a sequence of n
# elements. Given a sample sentence "This is a simple example", the letter 
# 2-grams are as follows: "th", "hi", "is", "s ", " i", "is", "s ", " a", "a ",
# " s", "si", "im", "mp", etc. The same sentence split into word 3-grams:
# "this is a", "is a simple", "a simple example".
# Another question is whether it really increases the accuracy of attribution;
# further reading: Eder, M. (2011). Style-markers in authorship attribution: 
# A cross-language study of the authorial fingerprint, "Studies in Polish
# Linguistics" 6: 101-16.
# Two types of n-grams are available: letters (option "l"), and words ("w").

analyzed.features = "w"
ngram.size = 1



#######  MATHEMATICAL SETTINGS (CLASSIFICATION METHOD)  #############


# method of classification: choose one of the options described below
# Delta ("delta"), k-nearest neighbor classification ("knn"),
# Naive Bayes classification ("naivebayes"), Nearest Shrunken Centroids
# ("nsc"), or Support Vectors Machines ("svm")
classification.method = "knn"



#######  MATHEMATICAL SETTINGS (DISTANCE MEASURE)  #################

# Strictly speaking, the choice of an appropriate distance measure
# is the core of the statistical procedure provided by this script.
# (However, the distance measures do not apply to the PCA method)
# Although this choice is not easy, some of the following measures
# seem to be more suitable for linguistic purposes than others.
# On theoretical grounds, Euclidean Distance and Manhattan
# Distance should be avoided in stylometry. Canberra Distance is quite 
# troublesome but effective e.g. for Latin (it should be combined with 
# careful culling settings and a limited number of MFW taken into analysis). 
# For English, usually Classic Delta is a good choice. A theoretical 
# explanation of the measures implemented in this script is forthcoming (?).
#
# The available distance measures (choose ONE) are as follows:
#   "CD" --> Classic Delta as developed by Burrows
#   "AL" --> Argamon's Linear Delta (based on Euclidean principles)
#   "ED" --> Eder's Delta (explanation and mathematical equation: soon)
#   "ES" --> Eder's Simple (explanation and mathematical equation: soon)
#   "MH" --> Manhattan Distance (obvious and well documented)
#   "CB" --> Canberra Distance (risky, but sometimes amazingly good)
#   "EU" --> Euclidean Distance (basic, and the most "natural")

distance.measure = "CD"



# this is obsolete, but still required somewhere below (bug to be fixed)
consensus.strength = 0.5



# Delta is always active: output is directed to a file. You may specify
# the number of final ranking candidates to be displayded (at least 1)

number.of.candidates = 3


# Report the number of correct guesses for each iteration (written to 
# the log file). Ranking of the least unlikely candidates in the log file.

how.many.correct.attributions = TRUE
final.ranking.of.candidates = TRUE


#######  ADVANCED SETTINGS (FOR EXPERTS ONLY)  ########################

# Normally, the script is computing a big table of thousands 
# of word frequencies. This is a non-trivial and time-consuming task.
# If done once, there is no need to waste time and do it again, because
# the tables are also written into output files. To retrieve all the word
# frequencies from existing files, switch this option to TRUE.
# BUT it MUST be set to FALSE when you switch corpora in the same R session!

use.existing.freq.tables = TRUE

# Some people like to see what's going on, and to be able to revise/edit
# the list of words for analysis. To meet their wishes, the script
# saves the list into a separate output file. You can either delete as many 
# words as you want from this file, or mark the unwanted words with "#" 
# (just like these comments are marked). Switching the following option on
# prevents the script from overwriting the file, and provides that the wordlist
# is loaded from there.

use.existing.wordlist = TRUE

# Usually, it is recommended to cut off the tail of the word-list;
# if you do not want to cut the list, then the variable may be set to an 
# absurdly big number, or to "mfw.list.cutoff = mfw.list.of.all"
# (and then you are advised to use a fast computer)

mfw.list.cutoff = 5000

# How the z-scores should be calculated:
# if the variable is set to FALSE, then the z-scores are relying
# on the primary set only (this should be better in most cases; after all,
# this is the classical solution used by Burrows and Hoover).
# Otherwise, the scaling is based on all the values
# in the primary and the secondary sets.

z.scores.of.all.samples = FALSE

# The both talbes of frequencies are build using the pre-prepared word
# list of the whole I set. Alternatively, one might want to prepare 
# this list of both sets. Similarily culling: it can be calcutated either 
# on the I set, or on both sets

reference.wordlist.of.all.samples = FALSE
culling.of.all.samples = TRUE


# file with the final ranking of Delta results (log file)

outputfile = "final_results.txt"


# when analyzed texts are significantly unequal in length, it is not a bad
# idea to prepare samples as randomly chosen "bags of words". If this option
# is switched on, the desired size of a sample should be indicated.
# Sampling with and without replacement is also available.
# (Further reading: Eder, M. (2010). Does Size Matter? Authorship Attribution,
# Short Samples, Big Problem. In "Digital Humanities 2010: Conference 
# Abstracts." King's College London 2010, pp. 132-35.)
#
# ATTENTION: this makes sense only if "use.existing.freq.tables" is set "FALSE"

random.sampling = FALSE
length.of.random.sample = 10000
sampling.with.replacement = FALSE

# the variables are now ready to use (unless the GUI option was chosen)
# ###################################################################








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







# #################################################
#
# the GUI module 
#
# #################################################

# At the beginning of the script, you could decide whether use the GUI module 
# or not; if the appropriate option was switched on, the GUI will start now

if (gui == TRUE) {
  library(tcltk)
  library(tcltk2)

if(file.exists("classify_config.txt") == TRUE) {
  source("classify_config.txt") }

# ###################################################################

.Tcl("font create myDefaultFont -family tahoma -size 8")
.Tcl("option add *font myDefaultFont")  
  cancel_pause <- FALSE
  tt <- tktoplevel()
  tktitle(tt) <- "Enter analysis parameters"

  push_OK <- function(){
      cancel_pause <<- TRUE
      tkdestroy(tt)
      }

corpus.format <- tclVar(corpus.format)
mfw.min <- tclVar(mfw.min)
mfw.max <- tclVar(mfw.max)
mfw.incr <- tclVar(mfw.incr)
start.at <- tclVar(start.at)
culling.min <- tclVar(culling.min)
culling.max <- tclVar(culling.max)
culling.incr <- tclVar(culling.incr)
ngram.size <- tclVar(ngram.size)
analyzed.features <- tclVar(analyzed.features)
classification.method <- tclVar(classification.method)
use.existing.freq.tables <- tclVar(use.existing.freq.tables)
use.existing.wordlist <- tclVar(use.existing.wordlist)
how.many.correct.attributions <- tclVar(how.many.correct.attributions)
mfw.list.cutoff <- tclVar(mfw.list.cutoff)
final.ranking.of.candidates <- tclVar(final.ranking.of.candidates)
delete.pronouns <- tclVar(delete.pronouns)
corpus.lang <- tclVar(corpus.lang)
distance.measure <- tclVar(distance.measure)
number.of.candidates <- tclVar(number.of.candidates)
random.sampling <- tclVar(random.sampling)
length.of.random.sample <- tclVar(length.of.random.sample)
sampling.with.replacement <- tclVar(sampling.with.replacement)
z.scores.of.all.samples <- tclVar(z.scores.of.all.samples)
reference.wordlist.of.all.samples <- tclVar(reference.wordlist.of.all.samples)
culling.of.all.samples <- tclVar(culling.of.all.samples)



# layout of the GUI begins here:
#
tkgrid(tklabel(tt,text="    ")) # blank line (serving as the top margin)



# first row: INPUT
#
entry_TXT <- tkradiobutton(tt)
entry_XML <- tkradiobutton(tt)
entry_XMLDrama <- tkradiobutton(tt)
entry_XMLNoTitles <- tkradiobutton(tt)
entry_HTML <- tkradiobutton(tt)
#
tkconfigure(entry_TXT,variable=corpus.format,value="plain")
tkconfigure(entry_XML,variable=corpus.format,value="xml")
tkconfigure(entry_XMLDrama,variable=corpus.format,value="xml.drama")
tkconfigure(entry_XMLNoTitles,variable=corpus.format,value="xml.notitles")
tkconfigure(entry_HTML,variable=corpus.format,value="html")
#
entrylabel_TXT <- tklabel(tt,text="plain text",anchor="w")
entrylabel_XML <- tklabel(tt,text="xml",anchor="w")
entrylabel_XMLDrama <- tklabel(tt,text="xml (plays)",anchor="w")
entrylabel_XMLNoTitles <- tklabel(tt,text="xml (notitles)",anchor="w")
entrylabel_HTML <- tklabel(tt,text="html",anchor="w")
#
tkgrid(tklabel(tt,text="  INPUT:           "),entrylabel_TXT,entrylabel_XML,entrylabel_XMLDrama,entrylabel_XMLNoTitles,entrylabel_HTML,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_TXT,entry_XML,entry_XMLDrama,entry_XMLNoTitles,entry_HTML,sticky="w")


# next row: LANGUAGE
#
entry_ENG <- tkradiobutton(tt)
entry_EN2 <- tkradiobutton(tt)
entry_EN3 <- tkradiobutton(tt)
entry_POL <- tkradiobutton(tt)
entry_LAT <- tkradiobutton(tt)
entry_LA2 <- tkradiobutton(tt)
entry_FRA <- tkradiobutton(tt)
entry_GER <- tkradiobutton(tt)
entry_HUN <- tkradiobutton(tt)
entry_ITA <- tkradiobutton(tt)
#
tkconfigure(entry_ENG,variable=corpus.lang,value="English")
tkconfigure(entry_EN2,variable=corpus.lang,value="English.contr")
tkconfigure(entry_EN3,variable=corpus.lang,value="English.all")
tkconfigure(entry_LAT,variable=corpus.lang,value="Latin")
tkconfigure(entry_LA2,variable=corpus.lang,value="Latin.corr")
tkconfigure(entry_POL,variable=corpus.lang,value="Polish")
tkconfigure(entry_FRA,variable=corpus.lang,value="French")
tkconfigure(entry_GER,variable=corpus.lang,value="German")
tkconfigure(entry_HUN,variable=corpus.lang,value="Hungarian")
tkconfigure(entry_ITA,variable=corpus.lang,value="Italian")
#
entrylabel_ENG <- tklabel(tt,text="English",anchor="w")
entrylabel_POL <- tklabel(tt,text="Polish",anchor="w")
entrylabel_LAT <- tklabel(tt,text="Latin",anchor="w")
entrylabel_FRA <- tklabel(tt,text="French",anchor="w")
entrylabel_GER <- tklabel(tt,text="German",anchor="w")
entrylabel_HUN <- tklabel(tt,text="Hungarian",anchor="w")
entrylabel_ITA <- tklabel(tt,text="Italian",anchor="w")
entrylabel_EN2 <- tklabel(tt,text="English (contr.)",anchor="w")
entrylabel_EN3 <- tklabel(tt,text="English (ALL)",anchor="w")
entrylabel_LA2 <- tklabel(tt,text="Latin (u/v > u)",anchor="w")
#
tkgrid(tklabel(tt,text="  LANGUAGE:        "),entrylabel_ENG,entrylabel_EN2,entrylabel_EN3,entrylabel_LAT,entrylabel_LA2,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_ENG,entry_EN2,entry_EN3,entry_LAT,entry_LA2,sticky="w")
tkgrid(tklabel(tt,text="                   "),entrylabel_POL,entrylabel_FRA,entrylabel_GER,entrylabel_HUN,entrylabel_ITA,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_POL,entry_FRA,entry_GER,entry_HUN,entry_ITA,sticky="w")
# Tooltips for the above
tk2tip(entrylabel_ENG, "Plain English: contractions and \ncompound words are split")
tk2tip(entrylabel_POL, "Plain Polish: contractions and \ncompound words are split")
tk2tip(entrylabel_EN2, "Modified English: \ncontractions are not split")
tk2tip(entrylabel_EN3, "Further Modified English: contractions \nand compound words are not split")
tk2tip(entrylabel_LAT, "Plain Latin: U and V \ntreated as distinct letters")
tk2tip(entrylabel_FRA, "Plain French: contractions and \ncompound words are split")
tk2tip(entrylabel_GER, "Plain German: contractions and \ncompound words are split")
tk2tip(entrylabel_HUN, "Plain Hungarian: contractions and \ncompound words are split")
tk2tip(entrylabel_ITA, "Plain Italian: contractions and \ncompound words are split")
tk2tip(entrylabel_LA2, "Modified Latin: U and V \nboth treated as U")


# next row: TEXT FEATURES

entry_W <- tkradiobutton(tt)
entry_L <- tkradiobutton(tt)
entry_NGRAMSIZE <- tkentry(tt,textvariable=ngram.size,width="8")
cb_RAND <- tkcheckbutton(tt)
#
tkconfigure(entry_W,variable=analyzed.features,value="w")
tkconfigure(entry_L,variable=analyzed.features,value="l")
tkconfigure(cb_RAND,variable=random.sampling)
entry_SIZE <- tkentry(tt,textvariable=length.of.random.sample,width="10")
#
entrylabel_W <- tklabel(tt,text="words",anchor="w")
entrylabel_L <- tklabel(tt,text="letters",anchor="w")
entrylabel_NGRAMSIZE <- tklabel(tt,text="ngram size")
cblabel_RAND <- tklabel(tt,text="Random sampling")
entrylabel_SIZE <- tklabel(tt,text="Random sample size  ")
#
tkgrid(tklabel(tt,text="  FEATURES:        "),entrylabel_W,entrylabel_L,entrylabel_NGRAMSIZE,entrylabel_LA2,cblabel_RAND,entrylabel_SIZE,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_W,entry_L,entry_NGRAMSIZE,entry_LA2,cb_RAND,entry_SIZE,sticky="w")
# Tooltips for the above
tk2tip(entrylabel_W, "Select this to work on words")
tk2tip(entrylabel_L, "Select this to work on letters \n(does not make much sense unless you use ngrams)")
tk2tip(entrylabel_NGRAMSIZE, "State your n for n-grams \nto work on word/letter clusters of n")
tk2tip(cblabel_RAND, "when the analyzed texts are significantly unequal in length, \nit is not a bad idea to prepare samples as randomly chosen *bags of words*. \nIf this option is switched on, the desired size of a sample should be indicated.")
tk2tip(entrylabel_SIZE, "Specify the random sample size. \nOnly relevant when random sampling is switched on.")



# next row: CLASSIFICATION METHOD
#
entry_DELTA <- tkradiobutton(tt)
entry_KNN <- tkradiobutton(tt)
entry_SVM <- tkradiobutton(tt)
entry_NBAYES <- tkradiobutton(tt)
entry_NSC <- tkradiobutton(tt)
#
tkconfigure(entry_DELTA,variable=classification.method,value="delta")
tkconfigure(entry_KNN,variable=classification.method,value="knn")
tkconfigure(entry_SVM,variable=classification.method,value="svm")
tkconfigure(entry_NBAYES,variable=classification.method,value="naivebayes")
tkconfigure(entry_NSC,variable=classification.method,value="nsc")
#
entrylabel_DELTA <- tklabel(tt,text="Delta",anchor="w")
entrylabel_KNN <- tklabel(tt,text="k-NN",anchor="w")
entrylabel_SVM <- tklabel(tt,text="SVM",anchor="w")
entrylabel_NBAYES <- tklabel(tt,text="NaiveBayes",anchor="w")
entrylabel_NSC <- tklabel(tt,text="NSC",anchor="w")
#
tkgrid(tklabel(tt,text="  CLASSIFIER:      "),entrylabel_DELTA,entrylabel_KNN,entrylabel_SVM,entrylabel_NBAYES,entrylabel_NSC,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_DELTA,entry_KNN,entry_SVM,entry_NBAYES,entry_NSC,sticky="w")
# Tooltips for the above
tk2tip(entrylabel_DELTA, "Burrows's Delta")
tk2tip(entrylabel_KNN, "k-Nearest Neighbor Classification")
tk2tip(entrylabel_SVM, "Support Vector Machines")
tk2tip(entrylabel_NBAYES, "Naive Bayes Classification. To use this method, you should have \nat least two texts of each class (author, genre, etc.) in the primary set")
tk2tip(entrylabel_NSC, "Nearest Shrunken Centroids Classification. To use this method, you should have \nat least two texts of each class (author, genre, etc.) in the primary set")

# next row: MFW SETTINGS
#
entry_MFW_MIN <- tkentry(tt,textvariable=mfw.min,width="8")
entry_MFW_MAX <- tkentry(tt,textvariable=mfw.max,width="8")
entry_MFW_INCR <- tkentry(tt,textvariable=mfw.incr,width="8")
entry_START_AT <- tkentry(tt,textvariable=start.at,width="8")
entry_CUT_OFF <- tkentry(tt,textvariable=mfw.list.cutoff,width="8")
#
entrylabel_MFW_MIN <- tklabel(tt,text="Minimum")
entrylabel_MFW_MAX <- tklabel(tt,text="Maximum")
entrylabel_MFW_INCR <- tklabel(tt,text="Increment")
entrylabel_START_AT <- tklabel(tt,text="Start at freq. rank")
entrylabel_CUT_OFF <- tklabel(tt,text="List Cutoff")
#
tkgrid(tklabel(tt,text="  MFW SETTINGS:    "),entrylabel_MFW_MIN,entrylabel_MFW_MAX,entrylabel_MFW_INCR,entrylabel_START_AT,entrylabel_CUT_OFF,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_MFW_MIN,entry_MFW_MAX,entry_MFW_INCR,entry_START_AT,entry_CUT_OFF,sticky="w")
# Tooltips for the above
tk2tip(entrylabel_MFW_MIN, "Set the minimum number of most frequent words. \nThe script will conduct its first analysis for \nthe number of words specified here")
tk2tip(entrylabel_MFW_MAX, "Set the maximum number of most frequent words. \nThe script will conduct its final analysis for \nthe number of words specified here")
tk2tip(entrylabel_MFW_INCR, "Set the increment added to \nthe minimum number of most frequent \nwords for each subsequent analysis.")
tk2tip(entrylabel_START_AT, "Set the number of words from the top of \nthe frequencyl ist to skip in the analysis.")
tk2tip(entrylabel_CUT_OFF, "Set the maximum size of the word frequency table. \nAnything above 5000 requires patience and a fast computer")


# next row: CULLING
#
cb_ALL_C <- tkcheckbutton(tt)
cb_DEL_PRON <- tkcheckbutton(tt)
#
entry_CUL_MIN <- tkentry(tt,textvariable=culling.min,width="8")
entry_CUL_MAX <- tkentry(tt,textvariable=culling.max,width="8")
entry_CUL_INCR <- tkentry(tt,textvariable=culling.incr,width="8")
tkconfigure(cb_DEL_PRON,variable=delete.pronouns)
tkconfigure(cb_ALL_C,variable=culling.of.all.samples)
#
entrylabel_CUL_MIN <- tklabel(tt,text="Minimum")
entrylabel_CUL_MAX <- tklabel(tt,text="Maximum")
entrylabel_CUL_INCR <- tklabel(tt,text="Increment")
cblabel_DEL_PRON <- tklabel(tt,text="Delete pronouns")
cblabel_ALL_C <- tklabel(tt,text="ALL culling")
#
tkgrid(tklabel(tt,text="  CULLING:         "),entrylabel_CUL_MIN,entrylabel_CUL_MAX, entrylabel_CUL_INCR,cblabel_DEL_PRON,cblabel_ALL_C,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_CUL_MIN,entry_CUL_MAX,entry_CUL_INCR,cb_DEL_PRON,cb_ALL_C,sticky="w")
# Tooltips for the above
tk2tip(entrylabel_CUL_MIN, "State the minimum culling setting. \n0 means no words are omitted from the analysis. \n50 means a words needs to appear in \nat least 50% of the texts to be included in the analysis. \n100 means that only words appearing in all the texts \nwill be included in the analysis")
tk2tip(entrylabel_CUL_MAX, "State the maximum culling setting. \n0 means no words are omitted from the analysis. \n50 means a words needs to appear in \nat least 50% of the texts to be included in the analysis. \n100 means that only words appearing in all the texts \nwill be included in the analysis")
tk2tip(entrylabel_CUL_INCR, "State the increment added to the minimum culling \nsetting for each subsequent analysis.")
tk2tip(cblabel_DEL_PRON, "Select if you want to omit pronouns in the analysis. \nThis improves attribution in some languages")
tk2tip(cblabel_ALL_C, "If this is left unchecked, the culling procedure \nis based on data from the primary set alone. \nChecking this box ensures words present in ALL texts \nof BOTH sets ar affected by the culling.")





# next row: VARIOUS
#
cb_QUEER <- tkcheckbutton(tt)
cb_GUESS <- tkcheckbutton(tt)
cb_FREQS <- tkcheckbutton(tt)
cb_LISTS <- tkcheckbutton(tt)
cb_ALL_L <- tkcheckbutton(tt)
#
tkconfigure(cb_QUEER,variable=final.ranking.of.candidates)
tkconfigure(cb_GUESS,variable=how.many.correct.attributions)
tkconfigure(cb_FREQS,variable=use.existing.freq.tables)
tkconfigure(cb_LISTS,variable=use.existing.wordlist)
tkconfigure(cb_ALL_L,variable=reference.wordlist.of.all.samples)
#
cblabel_QUEER <- tklabel(tt,text="Strange attributions")
cblabel_GUESS <- tklabel(tt,text="Count good guesses")
cblabel_FREQS <- tklabel(tt,text="Existing frequencies")
cblabel_LISTS <- tklabel(tt,text="Existing wordlist")
cblabel_ALL_L <- tklabel(tt,text="ALL wordlists")
#
tkgrid(tklabel(tt,text="  VARIOUS:         "),cblabel_QUEER,cblabel_GUESS,cblabel_FREQS,cblabel_LISTS,cblabel_ALL_L,sticky="w")
tkgrid(tklabel(tt,text="                   "),cb_QUEER,cb_GUESS,cb_FREQS,cb_LISTS,cb_ALL_L,sticky="w")
# Tooltips for the above
tk2tip(cblabel_QUEER, "Select to list misattributions in the final results file.")
tk2tip(cblabel_GUESS, "Select to count the proportion of correct attributions.")
tk2tip(cblabel_FREQS, "Select to use the frequency lists generated by the previous analysis. \nThis speeds up the process dramatically. \nA very bad idea if you've just changed your selection of texts!")
tk2tip(cblabel_LISTS, "Select to use the wordlist generated by \nthe previous analysis or a custom wordlist.")
tk2tip(cblabel_ALL_L, "If this is left unchecked, both frequency tables \nare built using a pre-prepared wordlist of the primary set. \nChecking this box compiles the list \nbasing on both primary and secondary sets.")


# next row: DELTA OPTIONS
#
entry_CD <- tkradiobutton(tt)
entry_AL <- tkradiobutton(tt)
entry_ED <- tkradiobutton(tt)
entry_ES <- tkradiobutton(tt)
entry_MH <- tkradiobutton(tt)
entry_CB <- tkradiobutton(tt)
entry_EU <- tkradiobutton(tt)
cb_ALL_Z <- tkcheckbutton(tt)
#
tkconfigure(entry_CD,variable=distance.measure,value="CD")
tkconfigure(entry_AL,variable=distance.measure,value="AL")
tkconfigure(entry_ED,variable=distance.measure,value="ED")
tkconfigure(entry_ES,variable=distance.measure,value="ES")
tkconfigure(entry_MH,variable=distance.measure,value="MH")
tkconfigure(entry_CB,variable=distance.measure,value="CB")
tkconfigure(entry_EU,variable=distance.measure,value="EU")
entry_CAND <- tkentry(tt,textvariable=number.of.candidates,width="8")
tkconfigure(cb_ALL_Z,variable=z.scores.of.all.samples)
#
entrylabel_CD <- tklabel(tt,text="Classic Delta")
entrylabel_AL <- tklabel(tt,text="Argamon's Delta")
entrylabel_ED <- tklabel(tt,text="Eder's Delta")
entrylabel_ES <- tklabel(tt,text="Eder's Simple")
entrylabel_MH <- tklabel(tt,text="Manhattan")
entrylabel_CB <- tklabel(tt,text="Canberra")
entrylabel_EU <- tklabel(tt,text="Euclidean")
entrylabel_CAND <- tklabel(tt,text="No. of candidates")
cblabel_ALL_Z <- tklabel(tt,text="ALL z-scores")
#
tkgrid(tklabel(tt,text="  DELTA OPTIONS:   "),entrylabel_CD,entrylabel_AL,entrylabel_ED,entrylabel_ES,cblabel_ALL_Z,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_CD,entry_AL,entry_ED,entry_ES,cb_ALL_Z,sticky="w")
tkgrid(tklabel(tt,text="                   "),entrylabel_MH,entrylabel_CB,entrylabel_EU,entrylabel_CAND,sticky="w")
tkgrid(tklabel(tt,text="                   "),entry_MH,entry_CB,entry_EU,entry_CAND,sticky="w")
# Tooltips for the above
tk2tip(entrylabel_CD, "Select the Classic Delta measure as developed by Burrows.")
tk2tip(entrylabel_AL, "Select Argamon's Linear Delta (based on Euclidean principles).")
tk2tip(entrylabel_ED, "Select Eder's Delta (explanation and mathematical equation: TBA).")
tk2tip(entrylabel_ES, "Select Eder's Simple measure (explanation and mathematical equation: TBA).")
tk2tip(entrylabel_MH, "Select Manhattan Distance (obvious and well documented).")
tk2tip(entrylabel_CB, "Select Canberra Distance (risky, but sometimes amazingly good).")
tk2tip(entrylabel_EU, "Select Euclidean Distance (basic and the most *natural*).")
tk2tip(entrylabel_CAND, "Set the number of candidates in the final results file.")
tk2tip(cblabel_ALL_Z, "If this is left unchecked, then the z-scores \nare based on the primary set only.")

# next row: ADVANCED
#
#
#
#
tkgrid(tklabel(tt,text="  SVM OPTIONS:     "),sticky="w")
tkgrid(tklabel(tt,text="  k-NN OPTIONS:    "),sticky="w")



# next row: the OK button
#
button_1 <- tkbutton(tt,text="     OK     ",command=push_OK,relief="groove")
tkbind(button_1,"<Return>",push_OK) 
tkgrid(button_1,columnspan="10")
tkgrid(tklabel(tt,text="    ")) # blank line (i.e., bottom margin)



##########

repeat{
  if(cancel_pause){
    analyzed.features <- as.character(tclvalue(analyzed.features))
    ngram.size <- as.numeric(tclvalue(ngram.size))
    corpus.format <- as.character(tclvalue(corpus.format))
    mfw.min <- as.numeric(tclvalue(mfw.min))
    mfw.max <- as.numeric(tclvalue(mfw.max))
    mfw.incr <- as.numeric(tclvalue(mfw.incr))
    start.at <- as.numeric(tclvalue(start.at))
    culling.min <- as.numeric(tclvalue(culling.min))
    culling.max <- as.numeric(tclvalue(culling.max))
    culling.incr <- as.numeric(tclvalue(culling.incr))
    classification.method <- as.character(tclvalue(classification.method))
    use.existing.freq.tables <- as.logical(as.numeric(tclvalue(use.existing.freq.tables)))
    use.existing.wordlist <- as.logical(as.numeric(tclvalue(use.existing.wordlist)))
    final.ranking.of.candidates <- as.logical(as.numeric(tclvalue(final.ranking.of.candidates)))
    how.many.correct.attributions <- as.logical(as.numeric(tclvalue(how.many.correct.attributions)))
    delete.pronouns <- as.logical(as.numeric(tclvalue(delete.pronouns)))
    number.of.candidates <- as.numeric(tclvalue(number.of.candidates))
    z.scores.of.all.samples <- as.logical(as.numeric(tclvalue(z.scores.of.all.samples)))
    reference.wordlist.of.all.samples <- as.logical(as.numeric(tclvalue(reference.wordlist.of.all.samples)))
    culling.of.all.samples <- as.logical(as.numeric(tclvalue(culling.of.all.samples)))
    random.sampling <- as.logical(as.numeric(tclvalue(random.sampling)))
    length.of.random.sample <- as.numeric(tclvalue(length.of.random.sample))
    mfw.list.cutoff <- as.numeric(tclvalue(mfw.list.cutoff))
    distance.measure <- as.character(tclvalue(distance.measure))
    corpus.lang <- as.character(tclvalue(corpus.lang))
    break
  }
.Tcl("font delete myDefaultFont")
}


} # <-- here the option "interactive.mode.with.GUI == TRUE" is completed

      
# #################################################
# GUI module explicit feliciter (Phew!)
# #################################################







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
# Function for combining single features (words
# or letters) into n-grams, or strings of n elements;
# e.g. letter 2-grams of the sentence "This is a sentence"
# are as follows: "th", "hi", "is", "s ", " i", "is", etc.
# Required argument: name of the vector of words/letters
# #################################################

make.ngrams = function(input.text) {
  txt = c()
  if(ngram.size > 1) {
    txt = input.text
    for(n in 2:ngram.size) {
    txt = paste(txt[1:(length(txt)-1)],input.text[n:length(input.text)])
    }
  } else {
  # if n-gram size is set to 1, then nothing will happen
  txt = input.text
  }
return(txt)
}




# #################################################
# Generic function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). Alternatively, 
# you can write here another rule for splitting.
# Required argument: name of the text to be split.
# ATTENTION: this is the only piece of coding in this script
# that dependens on the operating system used
# #################################################

split.into.words = function(input.text) {
  # splitting into units specified by regular expression;
	# here, all sequences between non-letter characters are assumed to be words:
	if(Sys.info()[["sysname"]] == "Windows") { 
	### Windows
		tokenized.text = c(unlist(strsplit(input.text, "\\W+|_+",perl=T)))
	} else {
	### Linux, Mac
		tokenized.text = c(unlist(strsplit(input.text, "[^[:alpha:]]+")))
	}
	# trying to avoid empty strings:
	tokenized.text = tokenized.text[nchar(tokenized.text)>0]
	# trying to get rid of non-letter characters:
	tokenized.text = tokenized.text[grep("[^[:digit:]]",tokenized.text)]
return(tokenized.txt)
}





# #################################################
# Function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). There is also an option
# of splitting the text into letters and/or performing
# splitting into n-grams
# #################################################

split.sample = function(input.text) {
  # loading the file; optionally, fiddling with dashes and contractions:
  #
  # this is the standard procedure of splitting input texts
  if(corpus.lang != "English.contr" && corpus.lang != "English.all") {
    tokenized.sample = split.into.words(input.text)
    }
  # if Latin option with adjusting the v/u letters was switched on,
  # smashing the distinction and converting both types to the letter u
  if(corpus.lang == "Latin.corr") {
    tokenized.sample = gsub("v","u",tokenized.sample)
    }    
  # this code is used for English corpora only
  if(corpus.lang == "English.contr" || corpus.lang == "English.all") {
    # replacing non-ASCII apostrophes with simple ' (standard ASCII char)
    tokenized.sample = gsub(iconv("\u2019",from="UTF-8"),"'",input.text)
    # getting rid of contractions ('t, 's, 've, 'd, 'll, 'em, 'im) by replacing
    # their apostrophes with ^ (other apostrophes will not be replaced)
    tokenized.sample = gsub("([[:alpha:]])'([tsdm]|ll|ve|em|im)\\b","\\1^\\2",
                            tokenized.sample)
    # adding spaces around dashes (to distinguish dashes and hyphens)
    tokenized.sample = gsub("[-]{2,5}"," -- ",tokenized.sample)
    # depending on which option was swithed on, either the contractions are
    # kept, or all the peculiarities, i.e. both contractions and hyphens
    if(corpus.lang == "English.contr") {
      tokenized.sample=c(unlist(strsplit(tokenized.sample,"[^[:alpha:]^]+")))
      }
    if(corpus.lang == "English.all") {
      tokenized.sample=c(unlist(strsplit(tokenized.sample,"[^[:alpha:]^-]+")))
      # trying to clean the remaining dashes:
      tokenized.sample = gsub("^[-]+$","",tokenized.sample)
      }
  }
  # trying to avoid empty strings:
  tokenized.sample = tokenized.sample[nchar(tokenized.sample)>0]
  # trying to get rid of non-letter characters:
  tokenized.sample = tokenized.sample[grep("[^[:digit:]]",tokenized.sample)]
  #
  #
  # splitting the sample into letters (if analyzed.features was set to "l")
  if(analyzed.features == "l") {
    tokenized.sample = paste(tokenized.sample, collapse=" ")
    tokenized.sample = unlist(strsplit(tokenized.sample,""))
    }
  #
  # making n-grams (if the value "n" has been set to 2 or more) 
  if(ngram.size > 1) {
  tokenized.sample = make.ngrams(tokenized.sample)
  }
# the result of the function:
return(tokenized.sample)
}



# #################################################
# Function for adjusting different input formats:
# xml (TEI) in two variants, html, and plain text files.
# Required argument: name of the text to pre-process
# #################################################

delete.markup = function(input.text) {
  if(corpus.format == "xml" || corpus.format == "xml.drama") {
    # getting rid of TEI header (if exists)
    if(length(grep("</teiheader>",input.text)) > 0) {
      input.text = input.text[-c(1:(grep("</teiheader>",input.text)))]
      }
    # the whole text into one (very) long line
    preprocessed.text = paste(input.text, collapse=" ")
    # getting rid of dramatis personae
    if(corpus.format == "xml.drama"){
      preprocessed.text = gsub("<speaker>.*?</speaker>","",preprocessed.text)
      }
    # getting rid of comments and (editorial) notes
    preprocessed.text = gsub("<note.*?</note>","",preprocessed.text)
    # getting rid of all the remaining tags
    preprocessed.text = gsub("<.*?>","",preprocessed.text)
  }
  if(corpus.format == "html") {
    # getting rid of html header (if exists)
    if(length(grep("<body",input.text)) > 0) {
      input.text = input.text[-c(1:(grep("<body",input.text)))]
      }
    # the whole text into one (very) long line
    preprocessed.text = paste(input.text, collapse=" ")
    # getting rid of links (menus and similar stuff should be deleted, hopefully)
    preprocessed.text = gsub("<a href.*?/a>","",preprocessed.text)
    # getting rid of all the remaining tags
    preprocessed.text = gsub("<.*?>","",preprocessed.text)
  } else {
  preprocessed.text = input.text
  }
return(preprocessed.text)
}




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
# loading the primary set from text files
corpus.of.primary.set = list()
setwd("primary_set")
  for (file in filenames.primary.set) {
  # loading the next file from the list filenames.primary.set,
  current.file = tolower(scan(file,what="char",sep="\n", quiet=T))
  # delete xml/html markup (if applicable)
  current.file = delete.markup(current.file)
  # deleting punctuation, splitting into words:
  split.file = split.sample(current.file)
  # if the current text is too short, abort the script
  if(length(split.file) < 10) {
    cat("\n\n",file, "\t", "this sample is VERY short!", "\n\n")
    setwd(".."); stop("corpus error")
    }
  # appending the current text to the virtual corpus
  corpus.of.primary.set[[file]] = split.file
  cat(file, "\t", "loaded successfully", "\n")
  }
setwd("..")
#
# loading the secondary set from text files
corpus.of.secondary.set = list()
setwd("secondary_set")
  for (file in filenames.secondary.set) {
  # loading the next file from the list filenames.secondary.set,
  current.file = tolower(scan(file,what="char",sep="\n", quiet=T))
  # delete xml/html markup (if applicable)
  current.file = delete.markup(current.file)
  # deleting punctuation, splitting into words:
  split.file = split.sample(current.file)
  # if the current text is too short, abort the script
  if(length(split.file) < 10) {
    cat("\n\n",file, "\t", "this sample is VERY short!", "\n\n")
    setwd(".."); stop("corpus error")
    }
  # appending the current text to the virtual corpus
  corpus.of.secondary.set[[file]] = split.file
  cat(file, "\t", "loaded successfully", "\n")
  }
setwd("..")
# blank line on the screen
cat("\n")
#
#
#
# both directories (primary_set and secondary_set) shoud contain some texts;
# if the number of text samples is lower than 2, the script will stop
if(length(corpus.of.primary.set) < 2 || length(corpus.of.secondary.set) < 2) {
    cat("\n\n","either primary_set or secondary_set is empty!", "\n\n")
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
  for (file in 1 : length(corpus.of.primary.set)) {
    # loading the next sample from the list filenames.primary.set,
    current.text = corpus.of.primary.set[[file]]
    # putting samples together:
    wordlist.of.primary.set = c(wordlist.of.primary.set, current.text)
    cat(names(corpus.of.primary.set[file]),"\t","tokenized successfully", "\n")
    }
# including words of the secondary set in the reference wordlist (if specified)
  if (reference.wordlist.of.all.samples == TRUE) {
    wordlist.of.secondary.set = c()
    for (file in 1 : length(corpus.of.secondary.set)) {
      # loading the next sample from the list filenames.secondary.set,
      current.text = corpus.of.secondary.set[[file]]
      # putting samples together:
      wordlist.of.secondary.set = c(wordlist.of.secondary.set, current.text)
    cat(names(corpus.of.secondary.set[file]),"\t","tokenized successfully","\n")
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
# #################################################
# FUNCTION: make.paralel.frequency.lists()
# preparing a huge table with all the frequencies (> mwf.list.cutoff).
# Two arguments are required -- a vector with filenames
# and a specified variable where the corpus is stored (in a list)
# #################################################
#
make.paralel.frequency.lists = function(filenames,current.corpus) {
  freq.list.of.all.the.samples = c()
  freq.list.of.current.sample = c()
    for (file in filenames) {
    # loading the next sample from the list filenames.primary.set,
    current.sample = current.corpus[[file]]
    #
    # if random sampling was chosen, the text will be randomized and 
    # a sample of a given lenght will be excerpted
    if(random.sampling == TRUE) {
    current.sample = head(sample(current.sample,
                        replace = sampling.with.replacement), 
                        length.of.random.sample)
    }
    #
    #
    # preparing the frequency list of the current sample
    raw.freq = table(current.sample) * 100 / length(current.sample)
    # adjusting the frequency list to the main MFW list obtained above
    freq.list.of.current.sample = raw.freq[mfw.list.of.all]
    # taking the names (sc. words) from the main MFW list 
    names(freq.list.of.current.sample) = mfw.list.of.all
    # and sticking the current sample into the general frequency table
    freq.list.of.all.the.samples = 
               rbind(freq.list.of.all.the.samples, freq.list.of.current.sample)
    # a short message on the screen:
    cat(file, "\t", "excerpted successfully", "\n")
  }
  # adjusting names of the rows (=samples)
  rownames(freq.list.of.all.the.samples) = c(filenames)
# the result of the function
return(freq.list.of.all.the.samples)
}
#
#
# preparing a huge table of all the frequencies for the primary set
freq.I.set.0.culling = 
  make.paralel.frequency.lists(filenames.primary.set,corpus.of.primary.set)
# all NA values will be adjusted to 0
freq.I.set.0.culling[which(is.na(freq.I.set.0.culling))] = 0
#
#
# preparing a huge table of all the frequencies for the secondary set
freq.II.set.0.culling = 
  make.paralel.frequency.lists(filenames.secondary.set,corpus.of.secondary.set)
# all NA values will be adjusted to 0
freq.II.set.0.culling[which(is.na(freq.II.set.0.culling))] = 0
#
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


# tu kod dla SVM, kNN, NSC, NaiveBayes
#
#
#
#
#
#
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



# a list of variables not to be removed
do.not.remove = c("zscores.table.both.sets", "freq.table.both.sets",
                  "freq.I.set.0.culling", "freq.II.set.0.culling",
                  "distance.table","outputfile","all.guesses")

# removing the variables which are not on the above list
list.of.variables = ls()
rm(list=list.of.variables[!(list.of.variables %in% do.not.remove)])



}

