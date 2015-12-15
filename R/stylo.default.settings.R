


stylo.default.settings = function(...) {


        

# if any command-line arguments have been passed by a user, they will
# be stored on the following list and used to overwrite the defaults
passed.arguments = list(...)




#######  TEXT- AND LANGUAGE-DEPENDENT SETTINGS  ####################

# format of corpus files; available choices are:
# "plain", "xml", "xml.drama", "xml.notitles", "html"
corpus.format = "plain"

# how many MFW ("Most frequent Words") should be taken into analysis 
# (if mfw.min value = max.mfw, then no multiple iterations will be computed)
# start.at option enables skipping top frequency words: you should
# indicate the desired start position of your list (in most cases you will 
# probably prefer setting it to 1, the rank of the single most frequent word,
# so that no words are skipped at the top of the frequency spectrum).

mfw.min = 100
mfw.max = 100
mfw.incr = 100

start.at = 1

# culling rate specifies the percentage of texts in a corpus in which a given word 
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
# If the "delete pronouns" option is switched to TRUE, choose one language
# of the following: English, Polish, Latin, French, German, Italian, Hungarian, Dutch, Spanish
# (the editable lists of pronouns are available below; see: advanced settings).
# Additionally, there are a few variants of language settings available:
# English.contr, English.all, and Latin.corr. Their meaning is as follows:
#     "English.contr": treats the contractions as single words, i.e. strings
#         such as "don't", "you've" etc. will not be split into two words.
#     "English.all": keeps the contractions (as above), and also prevents
#         from splitting compound words (mother-in-law, double-decker, etc.)
#     "Latin.corr": since some editions do not distinguish the letters v/u,
#         this option provides a consistent conversion to "u" in each text.

delete.pronouns = FALSE
corpus.lang = "English.all"

# using this variable, one can specify a selection of words to be EXCLUDED
# from the analysis: in computational linguistics, this is referred to 
# as stop words. It is not very likely to be used in a classical approach
# to stylometry, since the most frequent words the very features you 
# don't want to exclude
# usage: stop.words = c("the", "of", "in", "if")

stop.words = NULL

# Selection of features. In classical approaches, frequencies of the most
# frequent words (MFW) are used as the basis for multidimensional analyses.
# It has been argued, however, that other features are also worth considering,
# especially word and/or character n-grams. The general concept of n-grams
# is to divide a string of single words/characters into a sequence of n
# elements. Given a sample sentence "This is a simple example", the character 
# 2-grams are as follows: "th", "hi", "is", "s ", " i", "is", "s ", " a", "a ",
# " s", "si", "im", "mp", etc. The same sentence split into word 2-grams:
# "this is", "is a", "a simple", "simple sentence".
# Another question is whether it really increases the accuracy of attribution;
# further reading: Eder, M. (2011). Style-markers in authorship attribution: 
# A cross-language study of the authorial fingerprint. "Studies in Polish
# Linguistics" 6: 99-114.
# Two types of n-grams are available: characters (option "c"), and words ("w").

analyzed.features = "w"
ngram.size = 1
preserve.case = FALSE
encoding = "native.enc"


# some classifiers, e.g. Nearest Shrunken Centroids, allows you to 
# get the features that turned out to be most discriminative. Say TRUE
# if you want to have them

show.features = FALSE


#######  MATHEMATICAL SETTINGS (DISTANCE MEASURE)  #################

# Strictly speaking, the choice of the appropriate distance measure
# is the core of the statistical procedure provided by this script.
# (However, the distance measures do not apply to the PCA method)
# Although this choice is not easy, some of the following measures
# seem to be more suitable for linguistic purposes than others.
# On theoretical grounds, Euclidean Distance and Manhattan
# Distance should be avoided in stylometry. Canberra Distance is quite 
# troublesome but effective e.g. for Latin (it should be combined with 
# careful culling settings and a limited number of MFW taken into analysis). 
# For English, usually Classic Delta is a good choice. A theoretical 
# explanation of the measures implemented in this script is pending.
#
# The available distance measures (choose ONE) are as follows:
#   "delta" --> Classic Delta as developed by Burrows
#   "argamon" --> Argamon's Linear Delta (based on Euclidean principles)
#   "eder" --> Eder's Delta (explanation and mathematical equation: soon)
#   "simple" --> Eder's Simple (explanation and mathematical equation: soon)
#   "manhattan" --> Manhattan Distance (obvious and well documented)
#   "canberra" --> Canberra Distance (risky, but sometimes amazingly good)
#   "euclidean" --> Euclidean Distance (basic, and the most "natural")

distance.measure = "delta"



# Method of building a dendrogram; choose one of the following linkage methods:
# "nj", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", 
# "median", "centroid"
linkage = "ward.D"




########  VISUALIZATION METHODS, LOGS, REPORTS, DISPLAY OPTIONS  ############

# Statistical method to be used; choose one:
# Cluster Analysis: "CA"
# Multidimensional Scaling: "MDS"
# Principal Components Analysis (based on a covariance table): "PCV"
# Principal Components Analysis (based on a correlation table): "PCR"
# Bootstrap Consensus Tree: "BCT".
#
# Note on the bootstrap procedure: multiple iterations will build 
# a consensus tree
#   ATTENTION: this option requires the ape library, which you can install at
# any time using the "install.packages()" command.

consensus.strength = 0.5
analysis.type = "CA"


# Do you want to display the graph on the screen?
# Do you want to write the graph directly to a graphics file? Which format?
# You can display the graph on the screen AND write to a file (the latter 
# will be done with much better quality).

display.on.screen = TRUE
write.pdf.file = FALSE
write.jpg.file = FALSE
write.svg.file = FALSE
write.png.file = FALSE
dump.samples = FALSE

# dimensions of the plot area (expressed in inches), font size,
# thickness of the lines used to plot the graphs.
# Since it is usually hard to remember all the values, an additional option
# is provided to reset the picture options -- if this is switched on,
# the remaining options will be overwritten

plot.options.reset = FALSE
plot.custom.height = 7
plot.custom.width = 7
plot.font.size = 10
plot.line.thickness = 1

# custom offset between labels and point; custom margin size 
# (in percentage of plot area)
label.offset = 3
add.to.margins = 2

# how to represent samples on PCA or MDS graphs:
# "labels" || "points" || "both"
text.id.on.graphs = "labels"



# Do you want the graphs colored? The script will automatically assign 
# the same colors to those texts that have the same first segment of their 
# file names (the first string ending in "_").
# Available options: "colors" || "greyscale" || "black"

colors.on.graphs = "colors"

# Do you want titles on your graphs, listing the most important parameters?
titles.on.graphs = TRUE

# do you want to set your custom (main) title on the final picture?
# if not specified, the working directory name will be used as the title
custom.graph.title = NULL


# Layout of dendrogram: horizontal/vertical (Cluster Analysis only)

dendrogram.layout.horizontal = TRUE


# Initialize pca VISUALIZATION options; choose one:
# "classic", "loadings", "technical", "symbols"

pca.visual.flavour = "classic"


# Sometimes, you might want to save computed table(s) of distances
# for further analysis. Switch the following option TRUE to make it possible.
# The same applies to the list of words (or other features) actually used 
# in analysis, i.e. after culling, pronoun deletion, etc. Again, one might 
# want to save the table of frequencies actually used

save.distance.tables = FALSE
save.analyzed.features = FALSE
save.analyzed.freqs = FALSE


#######  ADVANCED SETTINGS (FOR EXPERTS ONLY)  ########################

# Normally, the script computes a huge table of thousands 
# of word frequencies for all texts in your corpus. This is a non-trivial 
# and time-consuming task. If done once, there is no need to waste time 
# and do it again, because the tables are also saved in the output file
# "table_with_frequencies.txt". To retrieve all the word
# frequencies from the file, switch this option to TRUE.
# BUT it MUST be set to FALSE when you switch corpora in the same R session, 
# or when you switch from word to character analysis, or change your n for
# your n-grams (or if you suddenly realize you've picked the
# wrong language!

use.existing.freq.tables = FALSE

# Some people like to see what's going on, and to be able to revise/edit
# the list of words for analysis. To meet their wishes, the script
# saves the list into a separate output file, "wordlist.txt".
# You can add any words to the list and either delete as many as you want, 
# or mark the unwanted words with "#" (just like these comments are marked). 
# Switching this option on prevents the script from overwriting the file, 
# and makes sure that the wordlist is loaded from there.

use.existing.wordlist = FALSE

# Otherwise, select files manually.

interactive.files = FALSE

# Another option makes it possible to upload the files using an external list
# of files. It should be named "files_to_analyze.txt" and be put into the working
# directory. The items (i.g. file names) should be separated either by spaces,
# tabs, or newlines. The delimiters can be mixed and/or multiplied, thus even
# a very untidy list will be interpreted correctly. 

use.custom.list.of.files = FALSE

# Usually, it is recommended to cut off the tail of the word-list;
# if you do not want to cut the list, then the variable may be set to an 
# absurdly big number, or to "mfw.list.cutoff = mfw.list.of.all"
# (and then you are advised to use a fast computer).

mfw.list.cutoff = 5000





#######  SAMPLING OPTIONS  ############################################

# "normal.sampling" || "random.sampling" || "no.sampling"
sampling = "no.sampling"

# When dealing with longer text, one might want to divide these in samples of 
# an equal size. This can be achieved by setting the sampling variable
# (default="normal.sampling") and specifying the number of words per sample 
# via the sample.size parameter: Integer, default=10000).

sample.size = 10000 # expressed in words, also if you're using char n-grams

# when the analyzed texts are significantly unequal in length, it is not a bad
# idea to prepare samples as randomly chosen "bags of words". For this, set the
# sampling variable to "random.sampling". The desired size of the sample should
# be indicated via the 'sample.size' variable, and the number of samples
# via the 'number.of.samples' parameter.
# Sampling with and without replacement is also available.
# (Further reading: Eder, M. (2010). Does Size Matter? Authorship Attribution,
# Short Samples, Big Problem. In "Digital Humanities 2010: Conference 
# Abstracts." King's College London 2010, pp. 132-35.)
#
# ATTENTION: this makes sense only if "use.existing.freq.tables" is set "FALSE"

sampling.with.replacement = FALSE # THIS IS NOT VISIBLE ON GUI

# It is also possible to use the entire corpus texts as samples (regardless 
# of their length and differences therein). For this, set the sampling variable 
# to "no.sampling"

sample.overlap = 0
number.of.samples = 1



#######  VARIOUS OPTIONS  ###############################################

# This option enables integration with TXM corpus management system
# (see: Textometrie Project, http://textometrie.ens-lyon.fr/).
# Normally, it should be switched off, since it is used only when the script
# is invoked from inside the TXM environment. WARNING: experimental.

txm.compatibility.mode = FALSE








#######  NETWORK ANALYSIS OPTIONS  ######################################


# Network analysis is not available "just like this" from inside the 'stylo' package;
# instead, it produces a table of edges (and, optionally, another table of nodes).

# An output file (or files) will be generated when this option is set TRUE;
# if this is set FALSE, next options are immaterial
network = TRUE

# Output format: either one table (edges), or two (edges and nodes);
# "edges" | "both"
network.tables = "edges"

# When "undirected" type of network is chosen (default), then the connections 'from'
# and 'to' are counted together (they are neither distinguished nor differentiated).
# When "directed" network is chosen, then the incoming connections and the outcoming
# ones are counted separately
network.type = "undirected"  # directed|undirected

# If this value is set to 1, then a link between a given sample and its nearest 
# neighbor is established; when it is set to 2, two neighbors are connected 
# (the nearest neighbor and the firs runner-up), etc. Default value is 3, which means
# that the nearest neighbor and two runners-up are taken into consideration
linked.neighbors = 3

# the connections' weights are differentiated: the nearest neighbor has the strongest
# link, then comes the first runner-up, and so forth. The difference between the 
# assigned weights might be "linear" = 1, 2, 3, ..., n; "quadratic" = 1, 4, 9, ..., n^2; 
# or "log" (logarithmic) = log(1+(1, 2, 3, ..., n))
# 
edge.weights = "linear" # "linear", "log", "quadratic"







#### additional variables for classify()

#######  VARIABLES FOR classify() (CLASSIFICATION METHOD)  #############

# method of classification: choose one of the options described below
# Delta ("delta"), k-nearest neighbor classification ("knn"),
# Naive Bayes classification ("naivebayes"), Nearest Shrunken Centroids
# ("nsc"), or Support Vectors Machines ("svm")
classification.method = "delta"

# Delta is always active: output is directed to a file. You may specify
# the number of final ranking candidates to be displayded (at least 1)
number.of.candidates = 3

# Report the number of correct guesses for each iteration (written to 
# the log file). Ranking of the least unlikely candidates in the log file.
how.many.correct.attributions = TRUE
final.ranking.of.candidates = TRUE


# NSC onlym, so far
show.features = FALSE


# SVM settings (refer to help(svm) from library(e1071) for details)
#
# kernel: choose linear, polynomial or radial:
# "linear" = u'*v 
# "polynomial" = (gamma*u'*v + coef0)^degree
# "radial" = exp(-gamma*|u-v|^2)
svm.kernel = "linear"
# degree: parameter needed for kernel of type "polynomial" (default: 3)
svm.degree = 3
# coef0: parameter needed for kernel of type "polynomial" (default: 0)
svm.coef0 = 0
# cost: cost of constraints violation (default: 1); it is the "C"-constant 
# of the regularization term in the Lagrange formulation.
svm.cost = 1



# k-NN settings:
#
# k value in k-NN, or number of neighbors considered:
k.value = 1
# minimum vote for definite decision, otherwise 'doubt'. (More precisely, 
# less than 'k-l' dissenting votes are allowed, even if k is increased by ties.)
l.value = 0


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

# file with the final ranking of classification results (log file)
outputfile = "final_results.txt"


# Deeper integration with R
#
# From the ver. 0.5.3, input data might be passed as R object,
# e.g. a corpus can be prepared (parsed) separately and attached at
# some point. It needs to have a form of a list containing particular samples
# and particular units (words, chars, POS tags) combined into n-grams.
# Also, an existing vector of features (an R object) can be used.
# If you have any table of frequencies stored in your R memory, you
# can skip the entire step of corpus preparation.
#
# these options have been moved directly to stylo() and classify()


# relative/raw frequencies
# in a vast majority of cases, relative freqs is the choice
relative.frequencies = TRUE



# Cross validation options: classify() only
# choose one: "none", "standard", "stratified"
# ['cv' is temporarily switched off, it always performs 'cv ="stratified"']
cv = "none"
cv.folds = 0
# no.of.samples.per.class



# #################################################
# OPPOSE settings -- to be integrated with stylo and classify!
# #################################################

text.slice.length = 3000
text.slice.overlap = 0

# if you want to ignore words that occurred only once or twice, set
# the threshold to 2; if you want to analyze all the words, set 0
rare.occurrences.threshold = 2

# choose one: "craig.zeta" | "eder.zeta" | "chisquare.zeta" | "mann.whitney" | "box.plot"
oppose.method = "craig.zeta"

### the meaning of the threshold parameter varies with the method chosen: #####
# "craig.zeta" method chosen, you might probably want to filter out some words
# of weak discrimination strength. Provided that 2 means the strongest 
# positive difference and O the strongest negative difference (Hoover 2009), 
# the values either just above or just below 1 are not significant and
# thus can be (or rather should be) omitted. If chisquare method was chosen,
# all the differences of p-value below 0.05 were filtered out, in pure Zeta,
# there is no a priori solution. Threshold 0.1 would filter out a vast majority
# of words, threshold set to 1 would filter all the words in a corpus.
zeta.filter.threshold = 0.1

# initialize the token to be plotted if the box.plot method is chosen
# [what about NULL?]
plot.token = ""

# these options can be deried from stylo.default.settings()
use.color.graphs = TRUE
polygons.on.graph = TRUE
identify.points = FALSE
classification = FALSE
naive.bayes = FALSE
svm.classification = TRUE
decision.tree.classification = FALSE
visualization = "words"

#################################################







# #################################################
# sanity check for some of the initial variables -- just in case
# #################################################



# For the sake of backward compatibility, obsolete distance names
# can be invoked by the user; they will be mapped into currently used ones

  if(distance.measure == "MH") {
        distance.measure = "manhattan"
    }
  if(distance.measure == "CB") {
        distance.measure = "canberra"
    }
  if(distance.measure == "EU") {
        distance.measure = "euclidean"
    }
  if(distance.measure == "CD") {
        distance.measure = "delta"
    }
  if(distance.measure == "AL") {
        distance.measure = "argamon"
    }
  if(distance.measure == "ED") {
        distance.measure = "eder"
    }
  if(distance.measure == "ES") {
        distance.measure = "simple"
    }

# This prevents us from choosing a non-existing distance measure -- in such
# case the default distance (Classic Delta) will be switched on. Be aware
# of correct spelling: then the default value will be assigned as well!
#if(distance.measure %in% c("CD","AL","ED","ES","MH","CB","EU") == FALSE) {
#  distance.measure = "CD"
#}



# in version R 3.0.4 the method "ward" has been replaced 
# by "ward.D" and "ward.D2"; this sanity check takes care 
# of passing the right argument
if(R.Version()$major <= 3 & R.Version()$minor <= 0.3) {
  if(linkage == "ward.D") {
    linkage = "ward"
  }
  if(linkage == "ward.D2") {
    linkage = "ward"
  }
} else {
  if(linkage == "ward") {
    linkage = "ward.D"
  }
}




# the final stage involves creating a list and putting all the variables on it;
# certainly, the list should not contain itself (!), it also should not 
# contain the index "i" used to generate the list
default.variables = list()
for(i in ls()[!ls() %in% c("i","default.variables")]) {
  default.variables[[i]] = get(i)
}




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
default.variables = merge.lists(default.variables, passed.arguments)






# the very final stage:
return(default.variables)
}
