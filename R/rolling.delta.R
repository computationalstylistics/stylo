
##############################################################################
# this is simply the Rollingdelta 0.0.8 script put into function(){ }

# it needs to be thoroughly re-written


rolling.delta = function(gui = TRUE, path = NULL,
                         primary.corpus.dir = "primary_set",
                         secondary.corpus.dir = "secondary_set") {



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





plot.options.reset = FALSE
plot.custom.height = 5
plot.custom.width = 10
plot.font.size = 10
plot.line.thickness = 1
pick.colors=TRUE
col1="blue"
col2="darkblue"
col3="green"
col4="aquamarine4"
col5="lightblue"
col6="cyan"
col7="brown"
col8="black"
col9="red"
col10="darkred"
col11="purple"
col12="orange"

# ############################################################################
# 
# ver. 0.0.8, 2013/05/27 --> script does not depend on operating system
# ver. 0.0.4, 2012/10/08 --> added option to save image files
# ver. 0.0.3, 2012/10/05 --> bug fixes
# ver. 0.0.2, 2012/10/04 --> bug fixes
# ver. 0.0.1, 2012/09/24 --> basic implementation, partially derived from
#                            previous scripts for Delta
##############################################################################

# TODO: something against too thick text slices (when a slice is longer than a whole text)



#######  GENERAL SETTINGS (GUI/TEXT-MODE)  ###################################

# If you wish to use a simple yet effective graphical interface (GUI),
# just set the following option to TRUE, otherwise switch this option to FALSE
# and edit manually the rest of variables (see below).
# If you switch this option on, the values indicated in the following sections 
# will serve as default for the GUI for the first run of the script on a corpus. 
# In the subsequent runs, last values will appear as default in the GUI.

interactive.mode.with.GUI = TRUE

##############################################################################



#######  SAMPLING PARAMETERS (SLICES etc.) ###################################

text.slice.length = 5000 # expressed in words, also if you're using character n-grams
text.slice.stepsize = 1000 # # expressed in words, also if you're using character n-grams

##############################################################################



#######  TEXT- AND LANGUAGE-DEPENDENT SETTINGS  ##############################

# format of corpus files; available choices are:
# "plain", "xml", "xml.drama", "xml.notitles", "html"
corpus.format = "plain"

# how many MFW ("Most frequent Words") should be taken into analysis 
# start.at option enables skipping top frequency words: you should
# indicate the desired start position of your list (in most cases you will 
# probably prefer setting it to 1, the rank of the single most frequent word,
# so that no words are skipped at the top of the frequency spectrum).

start.at = 1
mfw.max = 1000


# culling rate specifies the percentage of texts in a corpus in which a given word 
# must be found in order to be included in the analysis. Thus, a 100% culling 
# rate limits the analysis to words that appear at least once in every text 
# in the corpus; at a 50% culling rate, a word is included into the analysis 
# when it appears in at least half of the texts in the corpus; a 0% culling 
# rate (or no culling) means that no words are omitted.

culling = 100

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

delete.pronouns = TRUE
corpus.lang = "English.all"

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
# Linguistics" 6: 101-16.
# Two types of n-grams are available: characters (option "c"), and words ("w").

analyzed.features = "w"
ngram.size = 1

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

distance.measure = "CD"


########  VISUALIZATION METHODS, LOGS, REPORTS, DISPLAY OPTIONS  ############

# Do you want to display the graph on the screen?
# Do you want to write the graph directly to a graphics file? Which format?
# You can display the graph on the screen AND write to a file (the latter 
# will be done with much better quality).

display.on.screen = FALSE
write.pdf.file = FALSE
write.jpg.file = FALSE
write.svg.file = FALSE
write.png.file = TRUE

#############################################################################

#######  ADVANCED SETTINGS (FOR EXPERTS ONLY)  ########################


# Usually, it is recommended to cut off the tail of the word-list;
# if you do not want to cut the list, then the variable may be set to an 
# absurdly big number, or to "mfw.list.cutoff = mfw.list.of.all"
# (and then you are advised to use a fast computer).

mfw.list.cutoff = 5000


# pronouns (and other words) to be deleted
# * what are the selection criteria used here? Personal, possessive, ...? *

  # English
  eng.pronouns = c("he", "her", "hers", "herself", "him", "himself", "his", 
    "i", "me", "mine", "my", "myself", "our", "ours", "ourselves", "she", 
    "thee", "their", "them", "themselves", "they", "thou", "thy", "thyself", 
    "us", "we", "ye", "you", "your", "yours", "yourself")
  # Latin
  lat.pronouns = c("ea", "eae", "eam", "earum", "eas", "ego", "ei", "eis", 
    "eius", "eo", "eorum", "eos", "eum", "id", "illa", "illae", "illam", 
    "illarum", "illas", "ille", "illi", "illis", "illius", "illo", "illorum", 
    "illos", "illud", "illum", "is", "me", "mea", "meae", "meam", "mearum", 
    "meas", "mei", "meis", "meo", "meos", "meorum", "meum", "meus", "mihi", 
    "nobis", "nos", "noster", "nostra", "nostrae", "nostram", "nostrarum", 
    "nostras", "nostri", "nostris", "nostro", "nostros", "nostrorum", 
    "nostrum", "sua", "suae", "suam", "suarum", "suas", "sui", "suis", "suo", 
    "suos", "suorum", "suum", "suus", "te", "tibi", "tu", "tua", "tuae", 
    "tuam", "tuarum", "tuas", "tui", "tuis", "tuo", "tuos", "tuorum", "tuum", 
    "tuus", "vester", "vestra", "vestrae", "vestram", "vestrarum", "vestras", 
    "vestri", "vestris", "vestro", "vestros", "vestrorum", "vestrum", "vobis", 
    "vos")
  # French
  fra.pronouns = c("je", "me", "moi", "tu", "te", "toi", "il", "elle", "le", 
    "la", "lui", "se", "lui", "elle", "soi", "nous", "vous", "ils", "elles", 
    "les", "leur", "se", "eux", "elles", "soi")
  # German
  ger.pronouns = c("ich", "mich", "mir", "mein", "meine", "meiner", "meines", 
    "du", "dich", "dir", "dein", "deine", "deiner", "deines", "er", "sich", 
    "ihr", "ihrer", "ihn", "ihnen", "sein", "seiner", "seines", "seine", 
    "sie", "wir", "uns", "unser", "unsere", "euch", "eure", "euer")
  # Italian
  ita.pronouns = c("ci", "gli", "io", "la", "le", "lei", "li", "loro", "lo", 
    "lui", "me", "mi", "noi", "si", "te", "ti", "tu", "vi", "voi", "egli", 
    "ella", "esso", "essa", "essi", "esse", "mio", "mia", "miei", "mie", 
    "tuo", "tua", "tuoi", "tue", "suo", "sua", "suoi", "sue", "nostro", 
    "nostra", "nostri", "nostre", "vostro", "vostra", "vostri", "vostre", 
    "loro", "loro", "loro", "loro")
  # Polish
  pol.pronouns = c("ci", "ciebie", "ci\304\231", "go", "ich", "im", "ja", 
    "j\304\205", "je", "jego", "jej", "jemu", "ma", "m\304\205", "me", "mego", 
    "mej", "memu", "mi", "mn\304\205", "mnie", "moi", "moich", "moim", 
    "moimi", "moja", "moj\304\205", "moje", "mojego", "mojej", "mojemu", 
    "m\303\263j", "mu", "my", "mych", "mym", "mymi", "nam", "nami", "nas", 
    "ni\304\205", "nich", "nie", "niego", "niej", "niemu", "nim", "nimi", 
    "on", "ona", "one", "oni", "ono", "swa", "sw\304\205", "swe", "swego", 
    "swej", "swemu", "swoi", "swoich", "swoim", "swoimi", "swoja", 
    "swoj\304\205", "swoje", "swojego", "swojej", "swojemu", "sw\303\263j", 
    "swych", "swym", "swymi", "tob\304\205", "tobie", "twa", "tw\304\205", 
    "twe", "twego", "twej", "twemu", "twoi", "twoich", "twoim", "twoimi", 
    "twoja", "twoj\304\205", "twoje", "twojego", "twojej", "twojemu", 
    "tw\303\263j", "twych", "twym", "twymi", "ty", "wam", "wami", "was", 
    "wy", "wasz", "wasza", "wasze", "waszym", "waszymi", "waszych", 
    "waszego", "waszej", "wasz\304\205")
  # Hungarian
  hun.pronouns = c("annak", "az", "azzal", "bele", "bel\303\251", 
    "bel\303\251d", "bel\303\251je", "bel\303\251j\303\274k", "bel\303\251m", 
    "bel\303\251nk", "bel\303\251tek", "bel\303\266le", "bel\305\221led", 
    "bel\305\221lem", "bel\305\221letek", "bel\305\221l\303\274k", 
    "bel\305\221l\303\274nk", "benne", "benned", "bennem", "bennetek", 
    "benn\303\274k", "benn\303\274nk", "\303\251n", "ennek", "eny\303\251im", 
    "eny\303\251m", "eny\303\251mek", "\303\251rte", "\303\251rted", 
    "\303\251rtem", "\303\251rtetek", "\303\251rt\303\274k", 
    "\303\251rt\303\274nk", "ez", "ezzel", "hozz\303\241", "hozz\303\241d", 
    "hozz\303\241ja", "hozz\303\241juk", "hozz\303\241m", "hozz\303\241nk", 
    "hozz\303\241tok", "maga", "mag\303\241\303\251", "mag\303\241\303\251i", 
    "maguk", "maguk\303\251", "maguk\303\251i", "mi", "mieink", "mienk", 
    "mi\303\251nk", "n\303\241la", "n\303\241lad", "n\303\241lam", 
    "n\303\241latok", "n\303\241luk", "n\303\241lunk", "neked", "nekem", 
    "neki", "nekik", "nektek", "nek\303\274nk", "\305\221", "\305\221k", 
    "\303\266n", "\303\266n\303\251", "\303\266n\303\251i", "\303\266nnek", 
    "\303\266nnel", "\303\266n\303\266k", "\303\266n\303\266k\303\251", 
    "\303\266n\303\266k\303\251i", "\303\266n\303\266kkel", 
    "\303\266n\303\266knek", "\303\266v\303\251", "\303\266v\303\251i", 
    "\303\266v\303\251ik", "\303\266v\303\251k", "r\303\241d", "r\303\241ja", 
    "rajta", "rajtad", "rajtam", "rajtatok", "rajtuk", "rajtunk", 
    "r\303\241juk", "r\303\241m", "r\303\241nk", "r\303\241tok", 
    "r\303\263la", "r\303\263lad", "r\303\263lam", "r\303\263latok", 
    "r\303\263luk", "r\303\263lunk", "te", "ti", "tied", "ti\303\251d", 
    "tieid", "tieitek ", "tietek", "ti\303\251tek", "t\305\221le", 
    "t\305\221led", "t\305\221lem", "t\303\266letek", "t\305\221l\303\274k", 
    "t\305\221l\303\274nk", "vele", "veled", "velem", "veletek", 
    "vel\303\274k", "vel\303\274nk")
  # Dutch
  dut.pronouns = c("hij", "haar", "haarzelf", "hijzelf", "hemzelf", "hem", 
    "ik", "ikzelf", "mijn", "mij", "mijzelf", "me", "mezelf", "zich", 
    "zichzelf", "ons", "onze", "onszelf", "u", "uw", "uzelf", "zij", 
    "zijzelf", "wij", "wijzelf", "jij", "jijzelf", "jouw", "jouwe", "jou", 
    "jouzelf", "elkaar", "hen", "henzelf", "hun", "hunzelf", "zich", 
    "elkaar", "wie", "wat", "welke")
  # Spanish
  sp.pronouns = c("yo", "me", "m\303\255", "t\303\272", "te", "ti", "usted", 
    "ud", "le", "lo", "la", "se", "s\303\255", "\303\251l", "lo", "ella", 
    "nos", "nosotros", "nosotras", "vosotros", "vosotras", "ustedes", "ud", 
    "les", "los", "las", "se", "ellos", "los", "ellas")


# the variables are now ready to use (unless the GUI option was chosen)
# ###################################################################



# #################################################
# sanity check for some of the initial variables -- just in case
# #################################################

# Given a language option ("English", "Polish", "Latin" etc., as described 
# above), this procedure selects one of the lists of pronouns
# If no language was chosen (or if a desired language is not supported, or if 
# there was a spelling mistake), then the variable will be set to "English". 
# If "Pronouns deleted" is set to FALSE, this is immaterial.

if(exists("pronouns") == FALSE){ # checking if the "pronouns" box is empty
pronouns = eng.pronouns
}

# This prevents us from choosing a non-existing distance measure -- in such
# case the default distance (Classic Delta) will be switched on. Be aware
# of correct spelling: then the default value will be assigned as well!

if(distance.measure %in% c("CD","AL","ED","ES","MH","CB","EU") == FALSE) {
	distance.measure = "CD"
}


# #################################################
#
# the GUI module 
#
# #################################################

# At the beginning of the script, you could decide whether use the GUI module 
# or not; if the appropriate option was switched on, the GUI will start now;
# Since it's written in TclTk, with some additional twists, you need to install
# the tcltk2 package (on top of the regular tcltk, which is usually installed 
# with R anyway.

if (interactive.mode.with.GUI == TRUE) {
#	library(tcltk2)
	
	if(file.exists("config.txt") == TRUE) {
		source("config.txt") }
	
	.Tcl("font create myDefaultFont -family tahoma -size 8")
	.Tcl("option add *font myDefaultFont")  
	
	cancel_pause <- FALSE
	tt <- tktoplevel()
	tktitle(tt) <- "Rolling Delta | set parameters"
	
	push_OK <- function(){
		cancel_pause <<- TRUE
		tkdestroy(tt)
	}
	
	corpus.format <- tclVar(corpus.format)
	start.at <- tclVar(start.at)
	mfw.max <- tclVar(mfw.max)
	culling <- tclVar(culling)
	ngram.size <- tclVar(ngram.size)
	analyzed.features <- tclVar(analyzed.features)
	mfw.list.cutoff <- tclVar(mfw.list.cutoff)
	delete.pronouns <- tclVar(delete.pronouns)
	corpus.lang <- tclVar(corpus.lang)
	distance.measure <- tclVar(distance.measure)
	display.on.screen <- tclVar(display.on.screen)
	write.pdf.file <- tclVar(write.pdf.file)
	write.jpg.file <- tclVar(write.jpg.file)
	write.svg.file <- tclVar(write.svg.file)
	write.png.file <- tclVar(write.png.file)	
	text.slice.length <- tclVar(text.slice.length)
	text.slice.stepsize <- tclVar(text.slice.stepsize)
	plot.options.reset <- tclVar(plot.options.reset)
	plot.custom.height <- tclVar(plot.custom.height)
	plot.custom.width <- tclVar(plot.custom.width)
	plot.font.size <- tclVar(plot.font.size)
	plot.line.thickness <- tclVar(plot.line.thickness)
	col1 <- tclVar(col1)
	col2 <- tclVar(col2)
	col3 <- tclVar(col3)
	col4 <- tclVar(col4)
	col5 <- tclVar(col5)
	col6 <- tclVar(col6)
	col7 <- tclVar(col7)
	col8 <- tclVar(col8)
	col9 <- tclVar(col9)
	col10 <- tclVar(col10)
	col11 <- tclVar(col11)
	col12 <- tclVar(col12)
	
	f1 <- tkframe(tt)
	f2 <- tkframe(tt)
	f3 <- tkframe(tt)
	f4 <- tkframe(tt)
	f5 <- tkframe(tt)
	f6 <- tkframe(tt)
	
# layout of the GUI begins here:
	tab1 <- function() {
		tkgrid(f1,row=1,column=0,columnspan=6)
		tkgrid.forget(f2)
		tkgrid.forget(f3)
		tkgrid.forget(f4)
		tkgrid.forget(f5)
		tkgrid.forget(f6)
		tkconfigure(t1.but,state="disabled", background="white")
		tkconfigure(t2.but,state="normal", background="aliceblue")
		tkconfigure(t3.but,state="normal", background="aliceblue")
		tkconfigure(t4.but,state="normal", background="aliceblue")
		tkconfigure(t5.but,state="normal", background="aliceblue")
		tkconfigure(t6.but,state="normal", background="aliceblue")
	}
	tab2 <- function() {
		tkgrid(f2,row=1,column=0,columnspan=6)
		tkgrid.forget(f1)
		tkgrid.forget(f3)
		tkgrid.forget(f4)
		tkgrid.forget(f5)
		tkgrid.forget(f6)
		tkconfigure(t2.but,state="disabled", background="white")
		tkconfigure(t1.but,state="normal", background="aliceblue")
		tkconfigure(t3.but,state="normal", background="aliceblue")
		tkconfigure(t4.but,state="normal", background="aliceblue")
		tkconfigure(t5.but,state="normal", background="aliceblue")
		tkconfigure(t6.but,state="normal", background="aliceblue")
	}
	tab3 <- function() {
		tkgrid(f3,row=1,column=0,columnspan=6)
		tkgrid.forget(f1)
		tkgrid.forget(f2)
		tkgrid.forget(f4)
		tkgrid.forget(f5)
		tkgrid.forget(f6)
		tkconfigure(t3.but,state="disabled", background="white")
		tkconfigure(t1.but,state="normal", background="aliceblue")
		tkconfigure(t2.but,state="normal", background="aliceblue")
		tkconfigure(t4.but,state="normal", background="aliceblue")
		tkconfigure(t5.but,state="normal", background="aliceblue")
		tkconfigure(t6.but,state="normal", background="aliceblue")
	}
	tab4 <- function() {
		tkgrid(f4,row=1,column=0,columnspan=6)
		tkgrid.forget(f1)
		tkgrid.forget(f2)
		tkgrid.forget(f3)
		tkgrid.forget(f5)
		tkgrid.forget(f6)
		tkconfigure(t4.but,state="disabled", background="white")
		tkconfigure(t1.but,state="normal", background="aliceblue")
		tkconfigure(t2.but,state="normal", background="aliceblue")
		tkconfigure(t3.but,state="normal", background="aliceblue")
		tkconfigure(t5.but,state="normal", background="aliceblue")
		tkconfigure(t6.but,state="normal", background="aliceblue")
	}
	tab5 <- function() {
		tkgrid(f5,row=1,column=0,columnspan=6)
		tkgrid.forget(f1)
		tkgrid.forget(f2)
		tkgrid.forget(f3)
		tkgrid.forget(f4)
		tkgrid.forget(f6)
		tkconfigure(t5.but,state="disabled", background="white")
		tkconfigure(t1.but,state="normal", background="aliceblue")
		tkconfigure(t2.but,state="normal", background="aliceblue")
		tkconfigure(t3.but,state="normal", background="aliceblue")
		tkconfigure(t4.but,state="normal", background="aliceblue")
		tkconfigure(t6.but,state="normal", background="aliceblue")
	}
	tab6 <- function() {
		tkgrid(f6,row=1,column=0,columnspan=6)
		tkgrid.forget(f1)
		tkgrid.forget(f2)
		tkgrid.forget(f3)
		tkgrid.forget(f4)
		tkgrid.forget(f5)
		tkconfigure(t6.but,state="disabled", background="white")
		tkconfigure(t1.but,state="normal", background="aliceblue")
		tkconfigure(t2.but,state="normal", background="aliceblue")
		tkconfigure(t3.but,state="normal", background="aliceblue")
		tkconfigure(t4.but,state="normal", background="aliceblue")
		tkconfigure(t5.but,state="normal", background="aliceblue")
	}

	t1.but <- tkbutton(tt,text="     INPUT & LANGUAGE     ",command=tab1)
	t2.but <- tkbutton(tt,text="         FEATURES         ",command=tab2)
	t3.but <- tkbutton(tt,text="        STATISTICS        ",command=tab3)
	t4.but <- tkbutton(tt,text="         SAMPLING         ",command=tab4)
	t5.but <- tkbutton(tt,text="          OUTPUT          ",command=tab5)
	t6.but <- tkbutton(tt,text="          COLORS          ",command=tab6)
	tkgrid(t1.but)
	tkgrid(t2.but, column=1, row=0)
	tkgrid(t3.but, column=2, row=0)
	tkgrid(t4.but, column=3, row=0)
	tkgrid(t5.but, column=4, row=0)
	tkgrid(t6.but, column=5, row=0)	
# Grid for individual tabs
	
# initial state!
	tkgrid(f1,row=1,column=0,columnspan=6)
	tkconfigure(t1.but,state="disabled", background="white")
	tkconfigure(t2.but,state="normal", background="aliceblue")
	tkconfigure(t3.but,state="normal", background="aliceblue")
	tkconfigure(t4.but,state="normal", background="aliceblue")
	tkconfigure(t5.but,state="normal", background="aliceblue")
	tkconfigure(t6.but,state="normal", background="aliceblue")
# next row: the OK button
#
	button_1 <- tkbutton(tt,text="       OK       ",command=push_OK,relief="raised",background="aliceblue")
	tkbind(button_1,"<Return>",push_OK) 
	tkgrid(button_1,columnspan=6)
	tk2tip(button_1, "Press this only if you've visited all the tabs, or if you know\nyou want to leave values in some as they are.")
	
########################################################################################################################
# layout of the GUI begins here:
#
	tkgrid(tklabel(f1,text="    "),padx=0,pady=0) # blank line (serving as the top margin)
	tkgrid(tklabel(f2,text="    ")) # blank line (serving as the top margin)
	tkgrid(tklabel(f3,text="    ")) # blank line (serving as the top margin)
	tkgrid(tklabel(f4,text="    ")) # blank line (serving as the top margin)
	tkgrid(tklabel(f5,text="    ")) # blank line (serving as the top margin)
	tkgrid(tklabel(f6,text="    ")) # blank line (serving as the top margin)
# first row: INPUT
#
	entry_TXT <- tkradiobutton(f1)
	entry_XML <- tkradiobutton(f1)
	entry_XMLDrama <- tkradiobutton(f1)
	entry_XMLNoTitles <- tkradiobutton(f1)
	entry_HTML <- tkradiobutton(f1)
#
	tkconfigure(entry_TXT,variable=corpus.format,value="plain")
	tkconfigure(entry_XML,variable=corpus.format,value="xml")
	tkconfigure(entry_XMLDrama,variable=corpus.format,value="xml.drama")
	tkconfigure(entry_XMLNoTitles,variable=corpus.format,value="xml.notitles")
	tkconfigure(entry_HTML,variable=corpus.format,value="html")
#
	entrylabel_TXT <- tklabel(f1,text="plain text")
	entrylabel_XML <- tklabel(f1,text="xml")
	entrylabel_XMLDrama <- tklabel(f1,text="xml (plays)")
	entrylabel_XMLNoTitles <- tklabel(f1,text="xml (no titles)")
	entrylabel_HTML <- tklabel(f1,text="html")
#
	tkgrid(tklabel(f1,text="       INPUT:"),entrylabel_TXT,entrylabel_XML,entrylabel_XMLDrama,entrylabel_XMLNoTitles,entrylabel_HTML,columnspan=1)
	tkgrid(tklabel(f1,text="            "),entry_TXT,entry_XML,entry_XMLDrama,entry_XMLNoTitles,entry_HTML,columnspan=1)
# Tooltips for the above
	tk2tip(entrylabel_TXT, "Plain text files. \nIf your corpus does not contain diacritics, no encoding is needed. \nOtherwise, use ANSI for Windows, UTF-8 for Mac/Linux.")
	tk2tip(entrylabel_XML, "XML: all tags and TEI headers are removed.")
	tk2tip(entrylabel_XMLDrama, "XML for plays: all tags, TEI headers, \nand speakers' names between <speaker>...</speaker> tags are removed.")
	tk2tip(entrylabel_XMLNoTitles, "XML contents only: all tags, TEI headers, \nand chapter/section (sub)titles between <head>...</head> tags are removed.")
	tk2tip(entrylabel_HTML, "HTML headers, menus, links and other tags are removed.")
	tkgrid(tklabel(f1,text="    ")) # blank line for aesthetic purposes
	
# next row: LANGUAGE
#
	entry_ENG <- tkradiobutton(f1)
	entry_EN2 <- tkradiobutton(f1)
	entry_EN3 <- tkradiobutton(f1)
	entry_POL <- tkradiobutton(f1)
	entry_LAT <- tkradiobutton(f1)
	entry_LA2 <- tkradiobutton(f1)
	entry_FRA <- tkradiobutton(f1)
	entry_GER <- tkradiobutton(f1)
	entry_HUN <- tkradiobutton(f1)
	entry_ITA <- tkradiobutton(f1)
	entry_DUT <- tkradiobutton(f1)
	entry_SPA <- tkradiobutton(f1)
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
	tkconfigure(entry_DUT,variable=corpus.lang,value="Dutch")
	tkconfigure(entry_SPA,variable=corpus.lang,value="Spanish")
#
	entrylabel_ENG <- tklabel(f1,text="    English     ")
	entrylabel_POL <- tklabel(f1,text="    Polish      ")
	entrylabel_LAT <- tklabel(f1,text="    Latin       ")
	entrylabel_FRA <- tklabel(f1,text="    French      ")
	entrylabel_GER <- tklabel(f1,text="    German      ")
	entrylabel_HUN <- tklabel(f1,text="   Hungarian    ")
	entrylabel_ITA <- tklabel(f1,text="    Italian     ")
	entrylabel_EN2 <- tklabel(f1,text="English (contr.)")
	entrylabel_EN3 <- tklabel(f1,text="  English (ALL) ")
	entrylabel_LA2 <- tklabel(f1,text="Latin (u/v > u) ")
	entrylabel_DUT <- tklabel(f1,text="     Dutch      ")
	entrylabel_SPA <- tklabel(f1,text="    Spanish     ")
#
	tkgrid(tklabel(f1,text="LANGUAGE: "),entrylabel_ENG,entrylabel_EN2,entrylabel_EN3,entrylabel_LAT,entrylabel_LA2)
	tkgrid(tklabel(f1,text="          "),entry_ENG,entry_EN2,entry_EN3,entry_LAT,entry_LA2)
	tkgrid(tklabel(f1,text="          "),entrylabel_POL,entrylabel_HUN,entrylabel_FRA,entrylabel_ITA,entrylabel_SPA)
	tkgrid(tklabel(f1,text="          "),entry_POL,entry_HUN,entry_FRA,entry_SPA,entry_ITA)
	tkgrid(tklabel(f1,text="          "),entrylabel_DUT,entrylabel_GER)
	tkgrid(tklabel(f1,text="          "),entry_DUT,entry_GER)
	tkgrid(tklabel(f1,text="    ")) # blank line for aesthetic purposes
	
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
	tk2tip(entrylabel_DUT, "Plain Dutch: contractions and \ncompound words are split")
	tk2tip(entrylabel_SPA, "Plain Castilian: contractions and \ncompound words are split")
	
# next row: TEXT FEATURES
	entry_W <- tkradiobutton(f2)
	entry_L <- tkradiobutton(f2)
	cb_NGRAMS <- tkcheckbutton(f2)
	entry_NGRAMSIZE <- tkentry(f2,textvariable=ngram.size,width="8")
#
	tkconfigure(entry_W,variable=analyzed.features,value="w")
	tkconfigure(entry_L,variable=analyzed.features,value="c")
#
	entrylabel_W <- tklabel(f2,text="words")
	entrylabel_L <- tklabel(f2,text="chars")
	entrylabel_NGRAMSIZE <- tklabel(f2,text="ngram size")
#
	tkgrid(tklabel(f2,text="        FEATURES:"),entrylabel_W,entrylabel_L,entrylabel_NGRAMSIZE)
	tkgrid(tklabel(f2,text="                 "),entry_W,entry_L,entry_NGRAMSIZE)
	
# Tooltips for the above
	tk2tip(entrylabel_W, "Select this to work on words")
	tk2tip(entrylabel_L, "Select this to work on characters \n(does not make much sense unless you use ngrams)")
	tk2tip(entrylabel_NGRAMSIZE, "State your n for n-grams \nto work on word/char clusters of n")
	tkgrid(tklabel(f2,text="    ")) # blank line for aesthetic purposes
	
# next row: MFW SETTINGS
#
	entry_START_AT <- tkentry(f2,textvariable=start.at,width="8")
	entry_MFW_MAX <- tkentry(f2,textvariable=mfw.max,width="8")

#
	entrylabel_START_AT <- tklabel(f2,text="   Start at freq. rank   ")
	entrylabel_MFW_MAX <- tklabel(f2,text="   Maximum   ")

#
	tkgrid(tklabel(f2,text="MFW SETTINGS:"),entrylabel_START_AT,entrylabel_MFW_MAX)
	tkgrid(tklabel(f2,text="             "),entry_START_AT,entry_MFW_MAX)
	tkgrid(tklabel(f2,text="    ")) # blank line for aesthetic purposes
	
# Tooltips for the above
	tk2tip(entrylabel_START_AT, "Set the number of words from the top of \nthe frequency list to skip in the analysis.")
	tk2tip(entrylabel_MFW_MAX, "Set the maximum number of most frequent words. \nThe script will conduct its final analysis for \nthe number of words specified here")

# next row: CULLING
#
	cb_DEL_PRON <- tkcheckbutton(f2)
#
	entry_CUL <- tkentry(f2,textvariable=culling,width="8")
	entry_CUT_OFF <- tkentry(f2,textvariable=mfw.list.cutoff,width="8")
	tkconfigure(cb_DEL_PRON,variable=delete.pronouns)
#
	entrylabel_CUL <- tklabel(f2,text="   Culling rate     ")
	entrylabel_CUT_OFF <- tklabel(f2,text="   List Cutoff     ")
	cblabel_DEL_PRON <- tklabel(f2,text="   Delete pronouns     ")
#
	tkgrid(tklabel(f2,text="         SELECTION:"),entrylabel_CUL, entrylabel_CUT_OFF,cblabel_DEL_PRON)
	tkgrid(tklabel(f2,text="                 "),entry_CUL,entry_CUT_OFF,cb_DEL_PRON)
	tkgrid(tklabel(f2,text="    ")) # blank line for aesthetic purposes
	
# Tooltips for the above  
	tk2tip(entrylabel_CUL, "State the culling setting. \n0 means no words are omitted from the analysis. \n50 means a word needs to appear in \nat least 50% of the texts to be included in the analysis. \n100 means that only words appearing in all the texts \nwill be included in the analysis")
	tk2tip(entrylabel_CUT_OFF, "Set the maximum size of the word frequency table. \nAnything above 5000 requires patience and a fast computer")
	tk2tip(cblabel_DEL_PRON, "Select if you want to omit pronouns in the analysis. \nThis improves attribution in some languages")

	tkgrid(tklabel(f2,text="    ")) # blank line for aesthetic purposes 
	
# next row: DISTANCES
#
	entry_CD <- tkradiobutton(f3)
	entry_AL <- tkradiobutton(f3)
	entry_ED <- tkradiobutton(f3)
	entry_ES <- tkradiobutton(f3)
	entry_MH <- tkradiobutton(f3)
	entry_CB <- tkradiobutton(f3)
	
	entry_EU <- tkradiobutton(f3)
#
	tkconfigure(entry_CD,variable=distance.measure,value="CD")
	tkconfigure(entry_AL,variable=distance.measure,value="AL")
	tkconfigure(entry_ED,variable=distance.measure,value="ED")
	tkconfigure(entry_ES,variable=distance.measure,value="ES")
	tkconfigure(entry_MH,variable=distance.measure,value="MH")
	tkconfigure(entry_CB,variable=distance.measure,value="CB")
	tkconfigure(entry_EU,variable=distance.measure,value="EU")
#
	entrylabel_CD <- tklabel(f3,text="Classic Delta")
	entrylabel_AL <- tklabel(f3,text="Argamon's Delta")
	entrylabel_ED <- tklabel(f3,text="Eder's Delta")
	entrylabel_ES <- tklabel(f3,text="Eder's Simple")
	entrylabel_MH <- tklabel(f3,text="Manhattan")
	entrylabel_CB <- tklabel(f3,text="Canberra")
	entrylabel_EU <- tklabel(f3,text="Euclidean")
#
	tkgrid(tklabel(f3,text="  DISTANCES:"),entrylabel_CD,entrylabel_AL,entrylabel_ED,entrylabel_ES)
	tkgrid(tklabel(f3,text="            "),entry_CD,entry_AL,entry_ED,entry_ES)
	tkgrid(tklabel(f3,text="            "),entrylabel_MH,entrylabel_CB,entrylabel_EU)
	tkgrid(tklabel(f3,text="            "),entry_MH,entry_CB,entry_EU)
	tkgrid(tklabel(f3,text="    ")) # blank line for aesthetic purposes
	
# Tooltips for the above
	tk2tip(entrylabel_CD, "Select the Classic Delta measure as developed by Burrows.")
	tk2tip(entrylabel_AL, "Select Argamon's Linear Delta (based on Euclidean principles).")
	tk2tip(entrylabel_ED, "Select Eder's Delta (explanation and mathematical equation: TBA).")
	tk2tip(entrylabel_ES, "Select Eder's Simple measure (explanation and mathematical equation: TBA).")
	tk2tip(entrylabel_MH, "Select Manhattan Distance (obvious and well documented).")
	tk2tip(entrylabel_CB, "Select Canberra Distance (risky, but sometimes amazingly good).")
	tk2tip(entrylabel_EU, "Select Euclidean Distance (basic and the most *natural*).")
	tkgrid(tklabel(f3,text="    ")) # blank line for aesthetic purposes
	
# next row: SAMPLING
	entry_SLICELENGTH <- tkentry(f4,textvariable=text.slice.length,width="10")
	entry_SLICESTEPSIZE <- tkentry(f4,textvariable=text.slice.stepsize,width="10")
	
	entrylabel_SLICELENGTH <- tklabel(f4,text="   Slice length    ")
	entrylabel_SLICESTEPSIZE <- tklabel(f4,text="     Stepsize    ")

	
# Position and display sampling parameters on the grid:
	tkgrid(tklabel(f4,text="    SAMPLING:"),entrylabel_SLICELENGTH, entrylabel_SLICESTEPSIZE)
	tkgrid(tklabel(f4,text="    "),entry_SLICELENGTH, entry_SLICESTEPSIZE)
	tkgrid(tklabel(f4,text="    ")) # blank line for aesthetic purposes
	
# Tooltips for the above
	tk2tip(entrylabel_SLICELENGTH, "How many words per slice?")
	tk2tip(entrylabel_SLICESTEPSIZE, "How many words of overlap between consecutive slices?")
	
# next row: OUTPUT
#
	cb_SCRN <- tkcheckbutton(f5)
	cb_PDF <- tkcheckbutton(f5)
	cb_JPG <- tkcheckbutton(f5)
	cb_SVG <- tkcheckbutton(f5)
	cb_PNG <- tkcheckbutton(f5)
	cb_PLOT.RESET <- tkcheckbutton(f5)	
#
	tkconfigure(cb_SCRN,variable=display.on.screen)
	tkconfigure(cb_PDF,variable=write.pdf.file)
	tkconfigure(cb_JPG,variable=write.jpg.file)
	tkconfigure(cb_SVG,variable=write.svg.file)
	tkconfigure(cb_PNG,variable=write.png.file)
	entry_PLOT.HEIGHT <- tkentry(f5,textvariable=plot.custom.height,width="8")
	entry_PLOT.WIDTH <- tkentry(f5,textvariable=plot.custom.width,width="8")
	entry_PLOT.FONT <- tkentry(f5,textvariable=plot.font.size,width="8")
	entry_PLOT.LINE <- tkentry(f5,textvariable=plot.line.thickness,width="8")
	tkconfigure(cb_PLOT.RESET,variable=plot.options.reset)
#
	cblabel_SCRN <- tklabel(f5, text="     Onscreen     ")
	cblabel_PDF <- tklabel(f5,text="       PDF        ")
	cblabel_JPG <- tklabel(f5,text="       JPG        ")
	cblabel_SVG <- tklabel(f5,text="       SVG        ")
	cblabel_PNG <- tklabel(f5,text="       PNG        ")
	cblabel_PLOT.RESET <- tklabel(f5,text="Set default")
	entrylabel_PLOT.HEIGHT <- tklabel(f5,text="Plot height")
	entrylabel_PLOT.WIDTH <- tklabel(f5,text="Plot width")
	entrylabel_PLOT.FONT <- tklabel(f5,text="Font size")
	entrylabel_PLOT.LINE <- tklabel(f5,text="Line width")
	
#
	tkgrid(tklabel(f5,text="    GRAPHS:"), cblabel_SCRN,cblabel_PDF, cblabel_JPG,cblabel_SVG,cblabel_PNG,columnspan=5)
	tkgrid(tklabel(f5,text="           "), cb_SCRN,cb_PDF,cb_JPG,cb_SVG,cb_PNG,columnspan=5)
	tkgrid(tklabel(f5,text="    ")) # blank line for aesthetic purposes
	tkgrid(tklabel(f5,text=" PLOT SIZE:"), cblabel_PLOT.RESET,entrylabel_PLOT.HEIGHT, entrylabel_PLOT.WIDTH,entrylabel_PLOT.FONT,entrylabel_PLOT.LINE,columnspan=5)
	tkgrid(tklabel(f5,text="           "),cb_PLOT.RESET,entry_PLOT.HEIGHT,entry_PLOT.WIDTH,entry_PLOT.FONT,entry_PLOT.LINE,columnspan=5)
	tkgrid(tklabel(f5,text="    ")) # blank line for aesthetic purposes
	
# Tooltips for the above
	tk2tip(cblabel_SCRN, "Select to have your diagram(s) displayed on R's standard graphics device.")
	tk2tip(cblabel_PDF, "Select to save your diagram(s) as (a) PDF file(s).")
	tk2tip(cblabel_JPG, "Select to save your diagram(s) as (a) JPG file(s).")
	tk2tip(cblabel_SVG, "Select to save your diagram(s) as (a) SVG file(s).")
	tk2tip(cblabel_PNG, "Select to save your diagram(s) as (a) PNG file(s). \nProbably the best option for quality.")
	tk2tip(cblabel_PLOT.RESET, "Restore graphic parameters to safe default values \n(7x7 inches, 10 points font size, normal line width).")
	tk2tip(entrylabel_PLOT.HEIGHT, "Set custom plot height (in inches).")
	tk2tip(entrylabel_PLOT.WIDTH, "Set custom plot width (in inches).")
	tk2tip(entrylabel_PLOT.FONT, "Set custom font size (in points).")
	tk2tip(entrylabel_PLOT.LINE, "Set custom line width \n(in R units, 1 is default).")	

# next row: COLORS
#
label1 <- tklabel(f6, text="Color 1")
label2 <- tklabel(f6, text="Color 2")
label3 <- tklabel(f6, text="Color 3")
label4 <- tklabel(f6, text="Color 4")
label5 <- tklabel(f6, text="Color 5")
label6 <- tklabel(f6, text="Color 6")
label7 <- tklabel(f6, text="Color 7")
label8 <- tklabel(f6, text="Color 8")
label9 <- tklabel(f6, text="Color 9")
label10 <- tklabel(f6, text="Color 10")
label11 <- tklabel(f6, text="Color 11")
label12 <- tklabel(f6, text="Color 12")
color.selection <- c("blue", "darkblue", "green", "aquamarine4", "lightblue", "cyan", "brown", "black", "red", "darkred", "purple", "orange")

col1 <- tclVar(col1)
col2 <- tclVar(col2)
col3 <- tclVar(col3)
col4 <- tclVar(col4)
col5 <- tclVar(col5)
col6 <- tclVar(col6)
col7 <- tclVar(col7)
col8 <- tclVar(col8)
col9 <- tclVar(col9)
col10 <- tclVar(col10)
col11 <- tclVar(col11)
col12 <- tclVar(col12)

# Default selections for the two combo boxes

col1 <- tclVar("blue")
col2 <- tclVar("darkblue")
col3 <- tclVar("green")
col4 <- tclVar("aquamarine4")
col5 <- tclVar("lightblue")
col6 <- tclVar("cyan")
col7 <- tclVar("brown")
col8 <- tclVar("black")
col9 <- tclVar("red")
col10 <- tclVar("darkred")
col11 <- tclVar("purple")
col12 <- tclVar("orange")

# 1st box
combo.1 <- ttkcombobox(f6, values=color.selection, textvariable=col1, state="normal", width=12) 
# 2nd box
combo.2 <- ttkcombobox(f6, values=color.selection, textvariable=col2, state="normal", width=12) 
# 3nd box
combo.3 <- ttkcombobox(f6, values=color.selection, textvariable=col3, state="normal", width=12) 
# 4th box
combo.4 <- ttkcombobox(f6, values=color.selection, textvariable=col4, state="normal", width=12) 
# 5th box
combo.5 <- ttkcombobox(f6, values=color.selection, textvariable=col5, state="normal", width=12) 
# 6th box
combo.6 <- ttkcombobox(f6, values=color.selection, textvariable=col6, state="normal", width=12) 
# 7th box
combo.7 <- ttkcombobox(f6, values=color.selection, textvariable=col7, state="normal", width=12) 
# 8th box
combo.8 <- ttkcombobox(f6, values=color.selection, textvariable=col8, state="normal", width=12) 
# 9th box
combo.9 <- ttkcombobox(f6, values=color.selection, textvariable=col9, state="normal", width=12) 
# 10th box
combo.10 <- ttkcombobox(f6, values=color.selection, textvariable=col10, state="normal", width=12) 
# 11th box
combo.11 <- ttkcombobox(f6, values=color.selection, textvariable=col11, state="normal", width=12) 
# 12th box
combo.12 <- ttkcombobox(f6, values=color.selection, textvariable=col12, state="normal", width=12) 
tkgrid(tklabel(f6,text="    ")) # blank line for aesthetic purposes
tkgrid(label1,label2,label3,label4,label5,label6)
tkgrid(combo.1,combo.2,combo.3,combo.4,combo.5,combo.6)
tkgrid(tklabel(f6,text="    ")) # blank line for aesthetic purposes
tkgrid(label7,label8,label9,label10,label11,label12)
tkgrid(combo.7,combo.8,combo.9,combo.10,combo.11,combo.12)
tkgrid(tklabel(f6,text="    ")) # blank line for aesthetic purposes

# next row: the OK button
	tkgrid(tklabel(tt,text="    ")) # blank line (i.e., bottom margin)
	
	
##########
	
	repeat{
		if(cancel_pause){
			analyzed.features <- as.character(tclvalue(analyzed.features))
			ngram.size <- as.numeric(tclvalue(ngram.size))
			corpus.format <- as.character(tclvalue(corpus.format))
			start.at <- as.numeric(tclvalue(start.at))
			mfw.max <- as.numeric(tclvalue(mfw.max))
			culling <- as.numeric(tclvalue(culling))
			delete.pronouns <- as.logical(as.numeric(tclvalue(delete.pronouns)))
			display.on.screen <- as.logical(as.numeric(tclvalue(display.on.screen)))
			write.pdf.file <- as.logical(as.numeric(tclvalue(write.pdf.file)))
			write.jpg.file <- as.logical(as.numeric(tclvalue(write.jpg.file)))
			write.svg.file <- as.logical(as.numeric(tclvalue(write.svg.file)))
			write.png.file <- as.logical(as.numeric(tclvalue(write.png.file)))
			text.slice.length <-as.numeric(tclvalue(text.slice.length))
			text.slice.stepsize <-as.numeric(tclvalue(text.slice.stepsize))
			mfw.list.cutoff <- as.numeric(tclvalue(mfw.list.cutoff))
			distance.measure <- as.character(tclvalue(distance.measure))
			corpus.lang <- as.character(tclvalue(corpus.lang))
			plot.options.reset <- as.logical(as.numeric(tclvalue(plot.options.reset)))
			plot.custom.height <- as.numeric(tclvalue(plot.custom.height))
			plot.custom.width <- as.numeric(tclvalue(plot.custom.width))
			plot.font.size <- as.numeric(tclvalue(plot.font.size))
			plot.line.thickness <- as.numeric(tclvalue(plot.line.thickness))
			col1 <- as.character(tclvalue(col1))
			col2 <- as.character(tclvalue(col2))
			col3 <- as.character(tclvalue(col3))
			col4 <- as.character(tclvalue(col4))
			col5 <- as.character(tclvalue(col5))
			col6 <- as.character(tclvalue(col6))
			col7 <- as.character(tclvalue(col7))
			col8 <- as.character(tclvalue(col8))
			col9 <- as.character(tclvalue(col9))
			col10 <- as.character(tclvalue(col10))
			col11 <- as.character(tclvalue(col11))
			col12 <- as.character(tclvalue(col12))
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


# The chosen language option should be followed by an assignment of 
# the appropriate set of pronouns. The following code is responsible for it

if(corpus.lang == "English")
pronouns = eng.pronouns
if(corpus.lang == "Polish")
pronouns = pol.pronouns 
if(corpus.lang == "Latin")
pronouns = lat.pronouns
if(corpus.lang == "Latin.corr")
pronouns = lat.pronouns
if(corpus.lang == "French")
pronouns = fra.pronouns
if(corpus.lang == "German" )
pronouns = ger.pronouns
if(corpus.lang == "Italian")
pronouns = ita.pronouns
if(corpus.lang == "Hungarian")
pronouns = hun.pronouns
if(corpus.lang == "Dutch")
pronouns = dut.pronouns


  # Windows users are a bit allergic to Unicode; let's make them happy
  # by converting the chosen set of pronouns to local encoding
  if(Sys.info()[["sysname"]] == "Windows") { 
    pronouns = iconv(pronouns, from="UTF-8")
  }


# Since it it not so easy to perform, say, 17.9 iterations, or analyze
# 543.3 words, the code below rounds off all numerical variables to 
# the nearest positive integers, to prevent you from making silly jokes 
# with funny settings. (OK, it is still possible to crash the script in 
# more ways than one, but you will have to find them on your own).

mfw.max = round(mfw.max)
start.at = round(start.at)
culling = round(culling)
mfw.list.cutoff = round(mfw.list.cutoff)
text.slice.length = round(text.slice.length)
text.slice.stepsize = round(text.slice.stepsize)
# resetting the default plot area (if an appropriate option has been chosen)
if(plot.options.reset == TRUE) {
  plot.custom.height = 7
  plot.custom.width = 7
  plot.font.size = 10
  plot.line.thickness = 1
  plot.options.reset = FALSE
  }


# Finally, we want to save some of the variable values for later use;
# they are automatically loaded into the GUI at the next run of the script.
cat("",file="config.txt",append=F)
var.name<-function(x) { 
	if(is.character(x)==TRUE) {
		cat(paste(deparse(substitute(x)),"=\"",x,"\"", sep=""),file="config.txt",sep="\n",append=T)
	} else {
		cat(paste(deparse(substitute(x)),x, sep="="),file="config.txt",sep="\n",append=T) }
} 
var.name(corpus.format)
var.name(corpus.lang)
var.name(analyzed.features)
var.name(ngram.size)
var.name(start.at)
var.name(mfw.max)
var.name(culling)
var.name(mfw.list.cutoff)
var.name(delete.pronouns)
var.name(distance.measure)
var.name(display.on.screen)
var.name(write.pdf.file)
var.name(write.jpg.file)
var.name(write.svg.file)
var.name(write.png.file)
var.name(text.slice.length)
var.name(text.slice.stepsize)
var.name(plot.options.reset)
var.name(plot.custom.height)
var.name(plot.custom.width)
var.name(plot.font.size)
var.name(plot.line.thickness)
# #############################################################################


# #############################################################################
# Function for combining single features (words
# or characters) into n-grams, or strings of n elements;
# e.g. character 2-grams of the sentence "This is a sentence"
# are as follows: "th", "hi", "is", "s ", " i", "is", etc.
# Required argument: name of the vector of words/chars
# ############################################################################
make.ngrams = function(input.text) {
	# Splitting the sample into chars (if analyzed.features was set to "c")
	if(analyzed.features == "c") {
		input.text = paste(input.text, collapse=" ")
		input.text = unlist(strsplit(input.text,""))
	}
	text = c()
	if(ngram.size > 1) {
		text = input.text
		for(n in 2:ngram.size) {
			text = paste(text[1:(length(text)-1)],input.text[n:length(input.text)])
		}
	} else {
	# if n-gram size is set to 1, then nothing will happen
	text = input.text
	}
	return(text)
}
#############################################################################


# #################################################
# Function for adjusting different input formats:
# xml (TEI) in two variants, html, and plain text files.
# Required argument: name of the text to pre-process
# #################################################
delete.markup = function(input.text) {
	if(corpus.format == "xml" || corpus.format == "xml.drama") {
		# getting rid of the TEI header (if it exists)
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



###############################################################################
# Function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). Alternatively, 
# you can write here another rule for splitting.
# Required argument: name of the text to be split
###############################################################################
tokenize.text = function(input.text) {
	# loading the file, splitting into pieces specified by regular expression;
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
}
################################################################################



###############################################################################
# Function for slicing the text
###############################################################################
cleanup.text = function(input.text) {
	# loading the file; optionally, fiddling with apostrophes and contractions:
	# this is the standard procedure of splitting input texts
	if(corpus.lang != "English.contr" && corpus.lang != "English.all") {
		tokenized.text = tokenize.text(input.text)
	}
	# if the Latin option with adjusting the v/u letters is on,
	# this smashes the distinction and converts both types to the letter u
	if(corpus.lang == "Latin.corr") {
		tokenized.text = gsub("v","u",tokenized.text)
	}
	# this code is used for English corpora only
	if(corpus.lang == "English.contr" || corpus.lang == "English.all") {
		# replacing non-ASCII apostrophes with simple ' (standard ASCII char)
		tokenized.text = gsub(iconv("\u2019",from="UTF-8"),"'",input.text)
		# getting rid of contractions ('t, 's, 've, 'd, 'll, 'em, 'im) by replacing
		# their apostrophes with ^ (other apostrophes will not be replaced);
		# Of course, if your corpus is Cockney, you should edit the 
		# "([tsdm]|ll|ve|em|im)" statement accordingly.
		tokenized.text = gsub("([[:alpha:]])'([tsdm]|ll|ve|em|im)\\b","\\1^\\2", tokenized.text) #'
		# adding spaces around dashes (to distinguish dashes and hyphens)
		tokenized.text = gsub("[-]{2,5}"," -- ",tokenized.text)
		# depending on which option was swithed on, either the contractions are
		# kept, or all the peculiarities, i.e. both contractions and hyphens
		if(corpus.lang == "English.contr") {
			tokenized.text=c(unlist(strsplit(tokenized.text,"[^[:alpha:]^]+")))
		}
		if(corpus.lang == "English.all") {
			tokenized.text=c(unlist(strsplit(tokenized.text,"[^[:alpha:]^-]+")))
			# trying to clean the remaining dashes:
			tokenized.text = gsub("^[-]+$","",tokenized.text)
		}
	}
	# trying to avoid empty strings:
	tokenized.text = tokenized.text[nchar(tokenized.text)>0]
	# trying to get rid of non-letter characters:
	tokenized.text = tokenized.text[grep("[^[:digit:]]",tokenized.text)]
	# sanity check for text length: abort if the current text is shorter
	# than the specified slice length of if stepsize > slice.length
	if (length(tokenized.text) < text.slice.length){
		cat("\n\n",file, "\t", "This file is too short, given the slice length you have specified", "\n\n")
		setwd("..")
		stop("Corpus error...")
	}
	if (text.slice.stepsize > text.slice.length){
		cat("\n\n",file, "\t", "This stepsize parameter is too large, given the slice length you have specified", "\n\n")
		setwd("..")
		stop("Corpus error...")	
	}
	# at this point, each text in the corpus has been tokenized
	# into an array of tokens which we can slice into consecutive 'rolling' windows
	return(tokenized.text)
}
	
################################################################################

# #############################################################################
# This is where the script actually starts...

#


# Checking whether the required files and subdirectory exist
if(file.exists("primary_set")==FALSE || file.exists("secondary_set")==FALSE) {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
		"Hey! The working directory should contain the subdirectories \"primary_set\" and \"secondary_set\"\n",
		"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    stop("Corpus prepared incorrectly")
}
# retrieving the sets
filenames.primary.set = list.files("primary_set")
filenames.secondary.set = list.files("secondary_set")
if(length(filenames.secondary.set) > 1)  {
    cat("\n\n !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
		"Ho! \"secondary_set\" should only contain a single file!\n",
		"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    stop("Corpus prepared incorrectly")
}
#
# loading the primary set from text files
corpus.of.primary.set = list()
setwd("primary_set")
cat("Loading primary texts...\n")
for (file in filenames.primary.set) {
	# loading the next file from the list filenames.primary.set,
	current.file = tolower(scan(file,what="char",sep="\n",quiet=T))
	# delete xml/html markup (if applicable)
	clean.text = delete.markup(current.file)
	# deleting punctuation, splitting into words:
	tokenized.text = cleanup.text(clean.text)
	# appending the current text to the virtual corpus
	corpus.of.primary.set[[file]] = tokenized.text
	cat("\t\t- ", file, " loaded successfully (",length(tokenized.text)," words)\n",sep="")
	if(length(tokenized.text) < text.slice.length) {
		error.message = TRUE
		setwd("..")
		cat("The above file is too short to be split into", 
		text.slice.length,"words\n")
		stop("Change your settings please!")
	}
}
setwd("..")
# loading the secondary_set
corpus.of.secondary.set = list()
events.names.in.secondary.text = list()
events.indices.in.secondary.text = list()
setwd("secondary_set")
cat("Loading secondary texts...\n")
for (file in filenames.secondary.set) {
	# note: this is a dummy loop since there can only be a single secondary text...
	# loading the next file from the list filenames.secondary.set,
	current.file = tolower(scan(file,what="char",sep="\n",quiet=T))
	# deleting punctuation, splitting into words:
	tokenized.text = tokenize.text(current.file)
	# extract events:
	for (c in 1:length(tokenized.text)){
		w = tokenized.text[c]
		if (substr(w, 0, 10) == "xmilestone"){
			current.event = gsub("xmilestone","",w)
			events.names.in.secondary.text = c(events.names.in.secondary.text, current.event)
			events.indices.in.secondary.text = c(events.indices.in.secondary.text, c)
		}
	}
	for (event.index in events.indices.in.secondary.text){	
		tokenized.text[[event.index]] <- "xyzmarker" # mark indices
	}
	tokenized.text <- tokenized.text[tokenized.text!="xyzmarker"] # clean up after the indices
	# appending the current text to the virtual corpus
	corpus.of.secondary.set[[file]] = tokenized.text
	cat("\t\t- ", file, " loaded successfully (",length(tokenized.text)," words)\n",sep="")
	if(length(tokenized.text) < text.slice.length) {
		error.message = TRUE
		setwd("..")
		cat("The above file is too short to be split into", text.slice.length,"words\n")
		stop("Change your settings please!")
	}
	max.lines = length(tokenized.text) 
}
setwd("..")
# send blank line on the screen
cat("\n")

primary.set.clean.text.names = c()
# slice primary_set
primary.set.slices = c()
for (text.name in filenames.primary.set){
	primary.set.clean.text.names = c(primary.set.clean.text.names, gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)", "", text.name))
	slice.counter = 0
	current.text = corpus.of.primary.set[[text.name]]
	start.index = 0
	end.index = text.slice.length
	while(end.index <= length(current.text)) {
		slice.counter = slice.counter+1
		current.slice = current.text[start.index:(end.index-1)]
		current.slice.name = paste(gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)","",text.name), "-", slice.counter, sep="")
		primary.set.slices[[current.slice.name]] = make.ngrams(current.slice)
		start.index = start.index+text.slice.stepsize
		end.index = end.index+text.slice.stepsize
	}
}

# slice secondary_set
secondary.set.slices = c()
secondary.set.middle.indices = c()
for (text.name in filenames.secondary.set){
	slice.counter = 0
	current.text = corpus.of.secondary.set[[text.name]]
	start.index = 0
	end.index = text.slice.length
	while(end.index <= length(current.text)) {
		current.slice = current.text[start.index:(end.index-1)]
		slice.counter = slice.counter+1
		current.slice.name = paste(gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)","",text.name), "-", slice.counter, sep="")
		secondary.set.slices[[current.slice.name]] = make.ngrams(current.slice)
		# get middle index (will be used for plotting)
		middle.index = start.index+(text.slice.length/2)
		secondary.set.middle.indices = c(secondary.set.middle.indices, middle.index)
		# increment
		start.index = start.index+text.slice.stepsize
		end.index = end.index+text.slice.stepsize
	}
}

# Extracting all the words used in the primary set
wordlist.of.primary.corpus = c()
for (slice.name in names(primary.set.slices)){
	wordlist.of.primary.corpus = c(wordlist.of.primary.corpus, primary.set.slices[[slice.name]])
}
# preparing a sorted frequency list of the whole set
mfw.list.of.all.primary.slices = sort(table(c(wordlist.of.primary.corpus)),decreasing=T)

# if the whole list is long, then cut off the tail, as specified in the GUI 
# by the cutoff value
if (length(mfw.list.of.all.primary.slices) > mfw.list.cutoff) {
    mfw.list.of.all.primary.slices = mfw.list.of.all.primary.slices[1:mfw.list.cutoff]
}
# the only thing we need are words ordered by frequency (no frequencies)
mfw.list.of.all.primary.slices = names(mfw.list.of.all.primary.slices)
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
cat(mfw.list.of.all.primary.slices, file="wordlist.txt", sep="\n",append=T)


make.slice.frequency.lists = function(slice.names,current.slice.set) {
	freq.list.of.all.the.slices = c()
	freq.list.of.current.slice = c()
    for (slice.name in slice.names) {
		# loading the next slice from the slice list
		current.slice = current.slice.set[[slice.name]]
		# preparing the frequency list of the current slice
		raw.freq = table(current.slice) * 100 / length(current.slice)
		# adjusting the frequency list to the main MFW list obtained above
		freq.list.of.current.slice = raw.freq[mfw.list.of.all.primary.slices]
		# taking the names (sc. words) from the main MFW list 
		names(freq.list.of.current.slice) = mfw.list.of.all.primary.slices
		# and inserting the current slice into the general frequency table
		freq.list.of.all.the.slices = rbind(freq.list.of.all.the.slices, freq.list.of.current.slice)
		# a short message on the screen:
		#    cat(file, "\t", "excerpted successfully", "\n")
		cat(".")
	}
	# adjusting names of the rows (= slices)
	rownames(freq.list.of.all.the.slices) = c(slice.names)
	# the result of the function
	return(freq.list.of.all.the.slices)
}
#


# preparing a huge table of all the frequencies for the primary set
primary.frequencies.0.culling = make.slice.frequency.lists(names(primary.set.slices),primary.set.slices)
# all NA values will be adjusted to 0
primary.frequencies.0.culling[which(is.na(primary.frequencies.0.culling))] = 0
#
# getting rid of zero values (this might happen in random sampling
# or when custom wordlist are used)
primary.frequencies.0.culling = primary.frequencies.0.culling[,grep("FALSE",(colSums(primary.frequencies.0.culling))==0)]

# preparing a huge table of all the frequencies for the secondary set
secondary.frequencies.0.culling = make.slice.frequency.lists(names(secondary.set.slices),secondary.set.slices)
# all NA values will be adjusted to 0
secondary.frequencies.0.culling[which(is.na(secondary.frequencies.0.culling))] = 0


# saving the original mfw.max value in mfw.max.original
# this is useful for subtitles of bootstrap graphs
mfw.max.original = mfw.max

# #################################################
# culling (happens on the primary set only)
# #################################################

# testing if desired culling settings are acceptable;
# if too large, it is set to maximum possible
if(culling > 100){
	culling = 100
	cat("Your culling parameter has been automatically adjusted...")
} else {
	if(culling < 0){
		culling = 0
	cat("Your culling parameter has been automatically adjusted...")
	}
}

raw.list.after.culling = c()

# extracting non-zero values from primary set frequency table,
nonzero.values = primary.frequencies.0.culling > 0

# counting of how many non-zero values there are
for (y in 1: length(nonzero.values[1,])) {
	raw.list.after.culling = c(raw.list.after.culling, 
							   (length(grep("TRUE",nonzero.values[,y])) / 
								length(nonzero.values[,y])) 
							   >= culling/100 
							   )
}
# a raw culling list has no word-identification; let's change it:
names(raw.list.after.culling) = colnames(primary.frequencies.0.culling)

# a simple sequence of words which were not culled
list.of.words.after.culling = 
c(names(raw.list.after.culling[grep("TRUE",raw.list.after.culling)]))

# procedure for deleting pronouns
if (delete.pronouns == TRUE) {
	list.of.words.after.culling = list.of.words.after.culling[!(list.of.words.after.culling %in% pronouns)]
}

# the above list-of-not-culled to be applied to both sets:
primary.set = primary.frequencies.0.culling[,c(list.of.words.after.culling)]
secondary.set = secondary.frequencies.0.culling[,c(list.of.words.after.culling)]

# Testing if the desired MFW number is acceptable,
# if MFW too large, it is set to maximum possible.
if(mfw.max > length(primary.set[1,])) {
	mfw.max = length(primary.set[1,])
	cat("Your mfw.max parameter has been automatically adjusted...")
}
# if too small, it is set to 1 (i.e., minimal value)
if(start.at < 1) {
	start.at = 1
	cat("Your start.at parameter has been automatically adjusted...")
}

# MFW set to mfw.max for a while (it will change later on)
mfw = mfw.max

# select the appropriate rank range from the frequency spectrum
primary.set = primary.set[,start.at:mfw.max]
secondary.set = secondary.set[,start.at:mfw.max]

cat("\n\n")
cat("Culling @ ", culling," | ","available words",mfw.max,"\n")

score.max = -1000000
score.min = 1000000
plot.scores.all = c()
not.plotted.yet = TRUE
for (n in primary.set.clean.text.names){
	plot.indices = c()
	plot.scores = c()
	current.primary = primary.set[gsub("-[0-9]+", "", dimnames(primary.set)[[1]]) == n,]
	cat("\t- calculating scores for",n,"... \n")
	# mean and standard dev. for each word (in primary set)
	primary.set.mean = c(sapply(as.data.frame(current.primary), mean))
	primary.set.sd = c(sapply(as.data.frame(current.primary), sd))
	for (k in 1:length(secondary.set.middle.indices)){
		difference = 0.0
		for (w in list.of.words.after.culling[start.at:mfw.max]){
			prim.f = primary.set.mean[[w]]
			sec.f = secondary.set[k,w]
			sd.f = primary.set.sd[[w]]
			diff = (abs(prim.f-sec.f)/sd.f)
			if (!is.nan(diff)){difference = difference+(abs(prim.f-sec.f))} # to avoid zero divisions etc.
		}
		plot.indices = c(plot.indices, secondary.set.middle.indices[[k]])
		plot.scores = c(plot.scores, difference)
		if (difference < score.min){
			score.min = difference
		}
		if (difference > score.max){
			score.max = difference
		}
	}
	plot.scores.all[[n]] = plot.scores
}

plot.current.task = function(){
	symbol.counter = 1
# symbols = c("+", "o", "!", "*", "#", "|", "=", "@", "$", "&")
if (pick.colors == FALSE) {
colors = c("blue", "darkblue", "green", "aquamarine4", "lightblue", "cyan", "brown", "black", "red", "darkred", "purple", "orange")
	} else {
	colors = c(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,col11,col12)
	}
for (n in primary.set.clean.text.names){
	if (not.plotted.yet == TRUE){
		plot(plot.indices, plot.scores.all[[n]], xlim=c(0,max.lines), lwd=0.7, ylim=c(score.min, score.max), 
		col=colors[symbol.counter], type="o", xlab="Word indices of windows", ylab="Delta(centroid, window)", xaxt="n")
		not.plotted.yet = FALSE
	} else {
		lines(plot.indices, plot.scores.all[[n]], type="o", lwd=0.7, col=colors[symbol.counter])
	}
	symbol.counter = symbol.counter+1
}

	if(length(events.indices.in.secondary.text) > 1){
		for (k in 1:length(events.indices.in.secondary.text)){
			abline(v=events.indices.in.secondary.text[[k]], lty=3)
		}
		axis(3, at=events.indices.in.secondary.text, las=2, labels=events.names.in.secondary.text, cex.axis=0.8)	
	}

	axis(1, at=seq(0,max.lines,by=5000))
	legend("topleft", primary.set.clean.text.names, cex=0.8, text.col=colors[1:length(primary.set.clean.text.names)])
#	if you wish to place the legend yourself, use:
#	legend(locator(1), primary.set.clean.text.names, cex=0.8, text.col=colors[1:length(primary.set.clean.text.names)])
}


# Graphic output
graph.title = gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)","",filenames.secondary.set[1])
  if(display.on.screen == TRUE) {
    plot.current.task()
    }
  if(write.pdf.file == TRUE) {
    pdf(file = paste(graph.title,"%03d",".pdf",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.jpg.file == TRUE) {
    jpeg(filename = paste(graph.title,"%03d",".jpg",sep=""),
         width=plot.custom.width,height=plot.custom.height,
         units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.svg.file == TRUE) {
    svg(filename=paste(graph.title,"%03d",".svg",sep=""), 
         width=plot.custom.width,height=plot.custom.height,
         pointsize=plot.font.size)
    plot.current.task()
    dev.off()
    }
  if(write.png.file == TRUE) {
    png(filename = paste(graph.title,"%03d",".png",sep=""), 
         width=plot.custom.width,height=plot.custom.height,
         units="in",res=300,pointsize=plot.font.size)
    plot.current.task()
    dev.off()

}







# #################################################
# praparing final resutls: building a class


# some fake output
variable.to.be.done = c(0,0,0,0)
yet.another.variable = "nothing to be shown"



if(exists("plot.indices")) {
  attr(plot.indices, "description") = "final results [tbd]"
}
if(exists("plot.scores.all")) {
  attr(plot.scores.all, "description") = "a list containing final results"
}
if(exists("variable.to.be.done")) {
  attr(variable.to.be.done, "description") = "so far, there's nothing here"
}
if(exists("yet.another.variable")) {
  attr(yet.another.variable, "description") = "another empty variable"
}


# creating an object (list) that will contain the final results,
# [so far, there's nothing to show, alas...]
# This list will be turned into the class "stylo.results"
results.rolling.delta = list()
# elements that we want to add on this list
variables.to.save = c("plot.indices", 
                      "plot.scores.all",
                      "variable.to.be.done",
                      "yet.another.variable")
# checking if they really exist; getting rid of non-existing ones:
filtered.variables = ls()[ls() %in% variables.to.save]
# adding them on the list
for(i in filtered.variables) {
  results.rolling.delta[[i]] = get(i)
}


# adding some information about the current function call
# to the final list of results
results.rolling.delta$call = match.call()
results.rolling.delta$name = call("rolling.delta")


# This assings the list of final results to the class "stylo.resutls";
# the same class will be used to handle the output of classify(),
# rolling.delta() and oppose(). See the files "print.stylo.results.R"
# and "summary.stylo.results.R" (no help files are provided, since
# these two functions are not visible for the users).
class(results.rolling.delta) <- "stylo.results"

# return the value of the function 
return(results.rolling.delta)
}
