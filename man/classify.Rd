\name{classify}
\alias{classify}
\title{Machine-learning classification}
\description{Function that performs a number of machine-learning methods
for classification used in computational stylistics: Delta (Burrows, 2002), 
k-Nearest Neighbors, Support Vector Machines, Naive Bayes, 
and Nearest Shrunken Centroids (Jockers and Witten, 2010). Most of the options 
are derived from the \code{stylo} function.
}
\usage{
classify(gui = TRUE, training.frequencies = NULL, test.frequencies = NULL,
         training.corpus = NULL, test.corpus = NULL, features = NULL, 
         path = NULL, training.corpus.dir = "primary_set",
         test.corpus.dir = "secondary_set", ...)
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical user interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{training.frequencies}{using this optional argument, one can 
    load a custom table containing frequencies/counts for several variables, 
    e.g. most frequent words, across a number of text samples (for the 
    training set). It can be either 
    an R object (matrix or data frame), or a filename containing 
    tab-delimited data. If you use an R object, make sure that the rows
    contain samples, and the columns -- variables (words). If you use
    an external file, the variables should go vertically (i.e. in rows):
    this is because files containing vertically-oriented tables are far 
    more flexible and easily editable using, say, Excel or any text editor. 
    To flip your table horizontally/vertically use the generic function 
  \code{t()}.}

  \item{test.frequencies}{using this optional argument, one can 
    load a custom table containing frequencies/counts for the 
    test set. Further details: immediately above.}

  \item{training.corpus}{another option is to pass a pre-processed corpus
    as an argument (here: the training set). It is assumed that this object 
    is a list, each element of which is a vector containing one tokenized 
    sample. The example shown below will give you some hints how to prepare 
    such a corpus. Also, refer to \code{help(load.corpus.and.parse)}}

  \item{test.corpus}{if \code{training.corpus} is used, then you should also 
    prepare a similar R object containing the test set.}

  \item{features}{usually, a number of the most frequent features (words,
    word n-grams, character n-grams) are extracted automatically from the
    corpus, and they are used as variables for further analysis. However,
    in some cases it makes sense to use a set of tailored features, e.g.
    the words that are associated with emotions or, say, a specific subset 
    of function words. This optional argument allows to pass either a
    filename containing your custom list of features, or a vector 
    (R object) of features to be assessed.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{training.corpus.dir}{the subdirectory (within the current working directory) that
    contains the training set, or the collection of texts used to exemplify 
    the differences between particular classes (e.g. authors or genres). The discriminating features extracted from this training material will be used during the testing procedure (see below). If not specified, the default subdirectory 
    \code{primary_set} will be used.}

  \item{test.corpus.dir}{the subdirectory (within the working directory) that
    contains the test set, or the collection of texts that are used to 
    test the effectiveness of the discriminative features extracted from 
    the training set. In the case of authorship attribution e.g., 
    this set might contain works of non-disputed authorship, in order to check 
    whether a classification procedure attribute the tets texts to their correct author. This set contains `new' or `unseen' 
    data (e.g. anonymous samples or samples of disputed authorship in the case of authorship studies). If not specified, the default subdirectory \code{secondary_set} will be used.}

  \item{...}{any variable as produced by \code{stylo.default.settings()}
    can be set here to overwrite the default values.}
}

\details{There are numerous additional options that are passed to 
this function; so far, they are all loaded when \code{stylo.default.settings()} 
is executed (it will be invoked automatically from inside this function);
the user can set/change them in the GUI.}

\value{
The function returns an object of the class \code{stylo.results}:
a list of variables, including tables of word frequencies, vector of features 
used, a distance table and some more stuff. Additionally, depending on which 
options have been chosen, the function produces a number of files used to save 
the results, features assessed, generated tables of distances, etc.
}

\references{
Eder, M. Kestemont, M. and Rybicki, J. (2013). Stylometry with R:
    a suite of tools. In: "Digital Humanities 2013: Conference Abstracts". 
    University of Nebraska-Lincoln, Lincoln, NE, pp. 487-89.

Burrows, J. F. (2002). "Delta": a measure of stylistic difference and 
    a guide to likely authorship. "Literary and Linguistic Computing", 
    17(3): 267-87.

Jockers, M. L. and Witten, D. M. (2010). A comparative study of machine 
    learning methods for authorship attribution. "Literary and Linguistic
    Computing", 25(2): 215-23.

Argamon, S. (2008). Interpreting Burrows's Delta: geometric and 
  probabilistic foundations. "Literary and Linguistic Computing", 
  23(2): 131-47. 
}

\author{Maciej Eder, Mike Kestemont, Jan Rybicki}

\seealso{
\code{\link{stylo}}, \code{\link{rolling.delta}}, \code{\link{oppose}}
}

\examples{
\dontrun{
# standard usage (it builds a corpus from a collection of text files):
classify()


# loading word frequencies from two tab-delimited files:
classify(training.frequencies = "table_with_training_frequencies.txt",
         test.frequencies = "table_with_test_frequencies.txt")

         
# using two existing sub-corpora (a list containing tokenized texts):
txt1 = c("now", "i", "am", "alone", "o", "what", "a", "slave", "am", "i")
txt2 = c("what", "do", "you", "read", "my", "lord")
  setTRAIN = list(txt1, txt2)
  names(setTRAIN) = c("hamlet_sample1","polonius_sample1")
txt4 = c("to", "be", "or", "not", "to", "be")
txt5 = c("though", "this", "be", "madness", "yet", "there", "is", "method")
txt6 = c("the", "rest", "is", "silence")
  setTEST = list(txt4, txt5, txt6)
  names(setTEST) = c("hamlet_sample2", "polonius_sample2", "uncertain_1")
classify(training.corpus = setTRAIN, test.corpus = setTEST)


# using a custom set of features (words, n-grams) to be analyzed:
my.selection.of.function.words = c("the", "and", "of", "in", "if", "into", 
                                   "within", "on", "upon", "since")
classify(features = my.selection.of.function.words)


# loading a custom set of features (words, n-grams) from a file:
classify(features = "wordlist.txt")


# batch mode, custom name of corpus directories:
my.test = classify(gui = FALSE, training.corpus.dir = "TrainingSet",
       test.corpus.dir = "TestSet")
summary(my.test)


# batch mode, character 3-grams requested:
classify(gui = FALSE, analyzed.features = "c", ngram.size = 3)

}
}
%\keyword{classification}
%\keyword{support vector machines}
%\keyword{nearest shrunken centroids}
%\keyword{naive Bayes}
%\keyword{Delta}
%\keyword{k-nearest neighbors}
