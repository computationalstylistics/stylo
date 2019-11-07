## 'stylo' news ##




### version 0.7.1, 2019/11/4
  * improvements in crossv(): confusion matrix fully operational
  * new funcion performance.measures(), providing recall, precision, f1, etc.
  * performance measures made available via classify()
  * new function size.penalize() to assess minimal sample size
  * extension to the generic plot() function, to plot size.penalize() results


### version 0.7.0, 2019/01/22
  * Unicode (UTF-8) made the default encoding, also for Windows


### version 0.6.9, 2019/01/20
  * check.encoding() and change.encoding() introduced
  * GUI allows for changing the working directory with one click
  * metadata handling through a dedicated variable
  * {Steffen Pielström joins!}


### version 0.6.8, 2018/06/14
  * support for JCK (Japanese-Chinese-Korean) significantly improved
  * a fix for exporting networks to Gephi ver. 0.9.2
  * support for rmarkdown: stylo(), classify(), oppose()


### version 0.6.7, 2018/05/12
  * supports the following taggers: TaKIPI (for Polish), Alpino (Dutch)
  * the Imposters method reimplemented, via the new function imposters()
  * fine tuning the parameters of the Imposters method via imposters.optimize()


### version 0.6.6, 2018/04/13
  * Cosine Delta implemented and aviable via GUI
  * Min-Max distance implemented
  * Entropy distance implemented


### version 0.6.5, 2017/11/03
  * support for interactive network visualisations via stylo.network()
  * corrected Spanish pronouns
  * fixes in documentation
  * countless minor fixes


### version 0.6.4, 2016/09/08
  * citation hint updated; to see the changes type: citation("stylo")
  * the impostors method almost implemented, see help(perform.impostors)
  * confusion table for supervised classification via classify()
  * a separate funtion for cross-validation, see help(crossv)
  * a significant change in SVM wrapper: the procedure automatically
    gets rid of the variables with all 0s in the training set
  * the file inst/CITATION updated to meet recent CRAN requirements
  * man files for perform.delta, perform.svm etc. updated: new executable
    examples added, so that one can perform a supervised test without any corpus
  * perform.knn(), perform.svm() etc. improved, in order to handle custom
    vectors of classes provided by a user
  * an improved output of the oppose() function


### version 0.6.3, 2015/12/20
  * significant performance improvement in make.table.of.frequencies()
  * PCA values (rotation, explained variance, etc.) saved in final results


### version 0.6.2, 2015/11/11
  * the package 'stringi' involved to optimize n-gram computing
  * three datasets added to the package
    - data(novels), a collection of 9 novels by
      the Bronte sisters and Jane Austen (full text)
    - data(galbraith), a table of frequencies of 26
      novels by 5 authors, including Galbraith's "Cacoo's Calling"
    - data(lee), a table of frequencies of 28 American
      novels by 8 authors, including the new novel by Harper Lee
  * new version of make.table.of.frequencies(),
    which speeds up the tasks radically
  * delete.markup(), delete.stop.words(), make.samples(),
    make.frequency.list(), txt.to.features(), txt.to.words.ext()
    remodelled so that can be applied to single texts and/or to corpora
  * countless improvements in most of the functions


### version 0.6.1, 2015/09/27
  * UTF-8 issue in txt.to.words.ext() fixed, according to the CRAN's request


### version 0.6.0, 2015/08/17
  * support for Georgian
  * plot size in rolling.classify() improved
  * distance measure engine thoroughly restructured
  * custom distance measures allowed
  * cosine distance introduced
  * new functions: dist.cosine(), dist.delta(), dist.argamon(),
                   dist.eder(), dist.simple()
  * extracting POS tags via the function parse.pos.tags()


### version 0.5.9-3, 2015/07/2
  * support for Coptic
  * customizable graphs size in rolling.classify()
  * custom graph filename
  * integration with CLARIN-PL stylometric infrastructure


### version 0.5.9, 2015/01/30
  * non-ASCII chars in the source code neutralized
    (required by CRAN)
  * random sampling substantially improved


### version 0.5.8-3, 2014/10/26
  * bug fixes: options for assign.plot.colors()


### version 0.5.8-2, 2014/10/19
  * bug fixes: 'start.at' parameter in stylo()


### version 0.5.8-1, 2014/09/23
  * bug fixes (mostly: colors on dendrograms)


### version 0.5.8, 2014/09/3
  * new sequential methods available: rolling SVM,
    rolling NSC, and rolling Delta
  * bug in load.corpus.and.parse() fixed
  * bug in rolling.delta() fixed
  * network related bug in stylo() neutralized
  * classification procedures as separate functions:
    perform.delta(), perform.svm(), perform.knn(),
    perform.naivebayes(), perform.nsc()
  * classification output enhanced
  * doc files for new functions added


### version 0.5.7, 2014/08/13
  * culling implemented as a separate function
  * custom stop words deletion: delete.stop.words()
  * a thoroughly re-written oppose() to use
    the same tokenizing, corpus loading, 
    sampling etc. functions as stylo() and classify()
  * zeta.chisquare(), zeta.craig(), and zeta.eder()
    derrived as separate functions
  * gui.oppose() derrived as a separate function
  * distinctive words visualization in oppose() improved
  * draw.polygons derrived as a separate function
    (hidden to the end user, though)
  * cross-validation in classify() improved
  * fixed bug in cross-validation for naivebayes
  * a very unpleasant bug in oppose() fixed: 
    the preferred and avoided words were calculated
    using the I set only
  * help files significatnly improved


### version 0.5.6, 2014/04/20
  * support for Unicode on Windows
  * support for a few non Latin scripts
  * experimental support for CJK (Chinese-Japanese-Korean)
  * the function txt.to.words() remodelled
  * loading corpus files improved
  * printing variables on screen improved
  * better class inheritance
  * an issue with hclust and "ward", "ward.D" fixed
  * man files extended and updated


### version 0.5.5, 2014/04/03
  * cross-validation in classify()
  * lots of bugs fixed


### version 0.5.4, 2014/02/25
  * tSNE implemented
  * preserve.case option
  * more flexible function for splitting input text


### version 0.5.3, 2014/01/2
  * custom regular expressions to tokenize input texts
  * support for external corpora or frequencies
  * support for external set of features (e.g. frequent words)
  * class "stylo.results" for formatting final results
  * class "stylo.corpus" for formatting loaded corpora
  * class "stylo.data" for formatting tables and vectors
  * PCA coordinates piped to final results
  * optional choice between relative/raw frequencies
  * xml support improved (bug fixed)
  * codepage bug in oppose() fixed


### version 0.5.2, 2013/09/07
  * CRAN-related issue with .Rbuildignore fixed
  * network analysis support significantly improved
  * improvements in man pages


### version 0.5.1, 2013/08/07
  * bug fixes, minor improvements
  * different options for k-NN and SVM
  * submitted to CRAN for the first time (!)


### version 0.5.0-58, 2013/08/06
  * batch mode improved
  * several clustering algorithms available


### version 0.5.0-50, 2013/07/24
  * man pages revised and improved


### version 0.5.0-49, 2013/07/18
  * poster presentation at DH2013 (Lincoln, NE)
  * minor improvements


### version 0.5.0-48, 2013/06/26
  * namespace issues solved
  * documentation corrected (typos)


### version 0.5.0-45, 2013/06/12
  * arguments can be passed from command-line
  * man pages cleaned and extended
  * global variables abandoned
  * innumerable minor improvements


### version 0.5.0-43, 2013/04/31
  * thousands of changes and improvements
  * documentation improved and augmented
  * stylo R package (un)officially released


### version 0.5.0-30, 2013/04/26
  * changes in names of some functions
  * code cleaning, improvements, improvements, ...


### version 0.5.0-23, 2013/05/24
  * first prototype of an R package


### version 0.5.0-1, 2013/04/03
  * first attempt to port the stylo script into R package


### version 0.4.9-2, 3013/05/27
  * code OS-independent
  * minor cleaning


### version 0.4.9-1, 2013/04/02
  * experimental support for network analysis (output to Gephi)
  * bugs fixed


### version 0.4.9, 2013/03/06  
  * added option to dump samples for closer post-analysis inspection


### version 0.4.8, 2012/12/29
  * customizable plot area, font size, etc.
  * thoroughly rewritten code for margins assignment
  * scatterplots represented either by points, or by labels, or by both
    (customizable label offset)
  * saving the words (features) actually used
  * saving the table of actually used frequencies


### version 0.4.7, 2012/11/25 
  * new output/input extensions: optional custom list of files 
    to be analyzed, saving distance table(s) to external files
  * support for TXM Textometrie Project
  * color cluster analysis graphs (at last!)


### version 0.4.6, 2012/09/09
  * code revised, cleaned, bugs fixed


### version 0.4.5-4, 2012/09/03
  * added 2 new PCA visualization flavors


### version 0.4.5-3, 2012/08/31
  * new GUI written


### version 0.4.5-2, 2012/08/27
  * added functionality for normal sampling


### version 0.4.5-1, 2012/08/22
  * support for Dutch added
  * {Mike Kestemont joins!}


### version 0.4.5, 2012/07/07
  * option for choosing corpus files
  * code cleaned; bugs fixed


### version 0.4.4, 2012/05/31
  * the core code rewritten
  * I/II set division abandoned
  * GUI remodeled
  * GUI tooltips added
  * different input formats supported (xml etc.)
  * config options loaded from external file
  * the code forked into (1) the Stylo script, supporting explanatory 
    analyses (MDS, Cons. Trees, ...), (2) the Classify script for 
    machine-learning methods (Delta, SVM, NSC, Bayes)

    
### version 0.4.3, 2012/04/28
  * feature selection (word and character n-grams)


### version 0.4.2, 2012/02/10
  * three ways of splitting words in English
  * bugs fixed
  * GUI code rearranged and simplified


### version 0.4.1, 2011/06/27
  * better output
  * better text files uploading
  * new options for culling and ranking of candidates


### version 0.4.0, 2011/06/20
  * the official world-premiere, at DH2011 (Stanford, CA)


### version 0.3.9b, 2011/06/1
  * the code simplified; minor cleaning


### version 0.3.9, 2011/05/21
  * uploading wordlist from external source
  * thousands of improvements
  * the code simplified


### version 0.3.8, 2010/11/01
  * skip top frequency words option added


### version 0.3.7, 2010/11/01
  * better graphs
  * attempt at better graph layout


### version 0.3.6, 2010/07/31
  * more graphic options
  * dozens of improvements


### version 0.3.5, 2010/07/19
  * module for color graphs
  * module for PCA


### version 0.3.4, 2010/07/12
  * module for uploading corpus files improved


### version 0.3.3, 2010/06/03
  * the core code simplified and improved (faster!)


### version 0.3.2, 2010/05/10
  * reordered GUI
  * minor cleaning


### version 0.3.1, 2010/05/10
  * the z-scores module improved


### version 0.3.0, 2009/12/26
  * better counter of "good guesses"
  * option for randomly generated samples
  * minor improvements


### version 0.2.99, 2009/12/25
  * platform-independent outputfile saving


### version 0.2.98, 2009/12/24
  * GUI thoroughly integrated with initial variables


### version 0.2.10, 2009/11/28
  * corrected MFW display in graph
  * more analysis description in outputfile


### version 0.2.9, 2009/11/22
  * auto graphs for MSD and CA


### version 0.2.8a, 2009/11/21
  * remodeled GUI


### version 0.2.8, 2009/11/20
  * GUI: radiobuttons, checkbuttons


### version 0.2.7, 2009/11/19
  * language-determined pronoun selection


### version 0.2.6, 2009/11/18
  * dialog box (GUI)
  * {Jan Rybicki joins!}


### version 0.2.5, 2009/11/16
  * module for different distance measures
  * thousands of improvements (I/O, interface, etc.)


### version 0.2.2, 2009/10/25
  * numerous little improvements
  * deleting pronouns


### version 0.2.1, 2009/08/23
  * module for culling
  * module for bootstrapping


### version 0.2.0, 2009/08/23
  * module for uploading plain text files


### version 0.1.9, 2009/08/1 
  * innumerable improvements
  * the code simplified
  * {this version was completed on a train from Leipzig 
    to Krakow (a looong trip...), after a very successful 
    R course taught by Stefen Gries at ESU "C&T",
    Leipzig, Germany (26-31/08/2009)}

    
### version 0.1.4, 2009/07/19
  * loop for different MFW settings


### version 0.0.1, 2009/07/01
  * some bash and awk scripts translated into R


