stylo.default.settings <-
function() {
# TODO: options for uploading custom settings



#######  TEXT- AND LANGUAGE-DEPENDENT SETTINGS  ####################

# format of corpus files; available choices are:
# "plain", "xml", "xml.drama", "xml.notitles", "html"
corpus.format <<- "plain"

# how many MFW ("Most frequent Words") should be taken into analysis 
# (if mfw.min value = max.mfw, then no multiple iterations will be computed)
# start.at option enables skipping top frequency words: you should
# indicate the desired start position of your list (in most cases you will 
# probably prefer setting it to 1, the rank of the single most frequent word,
# so that no words are skipped at the top of the frequency spectrum).

mfw.min <<- 100
mfw.max <<- 100
mfw.incr <<- 100

start.at <<- 1

# culling rate specifies the percentage of texts in a corpus in which a given word 
# must be found in order to be included in the analysis. Thus, a 100% culling 
# rate limits the analysis to words that appear at least once in every text 
# in the corpus; at a 50% culling rate, a word is included into the analysis 
# when it appears in at least half of the texts in the corpus; a 0% culling 
# rate (or no culling) means that no words are omitted.
# about min=max: see above

culling.min <<- 0
culling.max <<- 0
culling.incr <<- 20

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

delete.pronouns <<- FALSE
corpus.lang <<- "English.all"

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

analyzed.features <<- "w"
ngram.size <<- 1


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
#   "CD" --> Classic Delta as developed by Burrows
#   "AL" --> Argamon's Linear Delta (based on Euclidean principles)
#   "ED" --> Eder's Delta (explanation and mathematical equation: soon)
#   "ES" --> Eder's Simple (explanation and mathematical equation: soon)
#   "MH" --> Manhattan Distance (obvious and well documented)
#   "CB" --> Canberra Distance (risky, but sometimes amazingly good)
#   "EU" --> Euclidean Distance (basic, and the most "natural")

distance.measure <<- "CD"


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

consensus.strength <<- 0.5
analysis.type <<- "CA"


# Do you want to display the graph on the screen?
# Do you want to write the graph directly to a graphics file? Which format?
# You can display the graph on the screen AND write to a file (the latter 
# will be done with much better quality).

display.on.screen <<- TRUE
write.pdf.file <<- FALSE
write.jpg.file <<- FALSE
write.emf.file <<- FALSE    # Windows only
write.png.file <<- FALSE
dump.samples <<- FALSE

# dimensions of the plot area (expressed in inches), font size,
# thickness of the lines used to plot the graphs.
# Since it is usually hard to remember all the values, an additional option
# is provided to reset the picture options -- if this is switched on,
# the remaining options will be overwritten

plot.options.reset <<- FALSE
plot.custom.height <<- 7
plot.custom.width <<- 7
plot.font.size <<- 10
plot.line.thickness <<- 1

# custom offset between labels and point; custom margin size 
# (in percentage of plot area)
label.offset <<- 3
add.to.margins <<- 2

# how to represent samples on PCA or MDS graphs:
# "labels" || "points" || "both"
text.id.on.graphs <<- "labels"



# Do you want the graphs colored? The script will automatically assign 
# the same colors to those texts that have the same first segment of their 
# file names (the first string ending in "_").
# Available options: "colors" || "grayscale" || "black"

colors.on.graphs <<- "colors"

# Do you want titles on your graphs, listing the most important parameters?

titles.on.graphs <<- TRUE


# Layout of dendrogram: horizontal/vertical (Cluster Analysis only)

dendrogram.layout.horizontal <<- TRUE


# Initialize pca VISUALIZATION options; choose one:
# "classic", "loadings", "technical", "symbols"

pca.visual.flavour <<- "classic" # || "technical" || "symbols"


# Sometimes, you might want to save computed table(s) of distances
# for further analysis. Switch the following option TRUE to make it possible.
# The same applies to the list of words (or other features) actually used 
# in analysis, i.e. after culling, pronoun deletion, etc. Again, one might 
# want to save the table of frequencies actually used

save.distance.tables <<- FALSE
save.analyzed.features <<- FALSE
save.analyzed.freqs <<- FALSE


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

use.existing.freq.tables <<- FALSE

# Some people like to see what's going on, and to be able to revise/edit
# the list of words for analysis. To meet their wishes, the script
# saves the list into a separate output file, "wordlist.txt".
# You can add any words to the list and either delete as many as you want, 
# or mark the unwanted words with "#" (just like these comments are marked). 
# Switching this option on prevents the script from overwriting the file, 
# and makes sure that the wordlist is loaded from there.

use.existing.wordlist <<- FALSE

# Otherwise, select files manually.

interactive.files <<- FALSE

# Another option makes it possible to upload the files using an external list
# of files. It should be named "files_to_analyze.txt" and be put into the working
# directory. The items (i.g. file names) should be separated either by spaces,
# tabs, or newlines. The delimiters can be mixed and/or multiplied, thus even
# a very untidy list will be interpreted correctly. 

use.custom.list.of.files <<- TRUE

# Usually, it is recommended to cut off the tail of the word-list;
# if you do not want to cut the list, then the variable may be set to an 
# absurdly big number, or to "mfw.list.cutoff = mfw.list.of.all"
# (and then you are advised to use a fast computer).

mfw.list.cutoff <<- 5000








#######  SAMPLING OPTIONS  ############################################

# "normal.sampling" || "random.sampling" || "no.sampling"
sampling <<- "no.sampling"

# When dealing with longer text, one might want to divide these in samples of 
# an equal size. This can be achieved by setting the sampling variable
# (default="normal.sampling") and specifying the number of words per sample 
# via the sample.size parameter: Integer, default=10000).

sample.size <<- 10000 # expressed in words, also if you're using character n-grams

# when the analyzed texts are significantly unequal in length, it is not a bad
# idea to prepare samples as randomly chosen "bags of words". For this, set the
# sampling variable to "random.sampling". The desired size of the sample should
# be indicated via the length.of.random.sample variable.
# Sampling with and without replacement is also available.
# (Further reading: Eder, M. (2010). Does Size Matter? Authorship Attribution,
# Short Samples, Big Problem. In "Digital Humanities 2010: Conference 
# Abstracts." King's College London 2010, pp. 132-35.)
#
# ATTENTION: this makes sense only if "use.existing.freq.tables" is set "FALSE"

length.of.random.sample <<- 10000 # THIS IS IRRELEVANT! USE "SAMPLE.SIZE" INSTEAD
sampling.with.replacement <<- FALSE # THIS IS NOT VISIBLE ON GUI

# It is also possible to use the entire corpus texts as samples (regardless 
# of their length and differences therein). For this, set the sampling variable 
# to "no.sampling"


#######  VARIOUS OPTIONS  ###############################################

# This option enables integration with TXM corpus management system
# (see: Textometrie Project, http://textometrie.ens-lyon.fr/).
# Normally, it should be switched off, since it is used only when the script
# is invoked from inside the TXM environment. WARNING: experimental.

txm.compatibility.mode <<- FALSE


# COMPATIBILITY MODE: to make old experiments replicable

# original algorithm for estabilishing consensus trees, used in ver. < 0.4.8;
# this is still available: if you want to replicate your old tests, say TRUE
nj.consensus.tree <<- FALSE

# in ver. 0.4.7, color dendrograms were available; they were produced using
# the neighbor joining (NJ) algorithm. If you want to use it and/or you
# want to replicate your experiments performed using ver. 0.4.7, say TRUE
nj.cluster.analysis <<- FALSE
    ######################################
          # loading the required library
          if(nj.cluster.analysis == TRUE) {
            library(ape)
            }
    ######################################




# the variables are now ready to use
# ###################################################################



# #################################################
# sanity check for some of the initial variables -- just in case
# #################################################


# This prevents us from choosing a non-existing distance measure -- in such
# case the default distance (Classic Delta) will be switched on. Be aware
# of correct spelling: then the default value will be assigned as well!

if(distance.measure %in% c("CD","AL","ED","ES","MH","CB","EU") == FALSE) {
  distance.measure <<- "CD"
}

}
