\name{rolling.classify}
\alias{rolling.classify}
\title{Sequential machine-learning classification}
\description{Function that splits a text into equal-sized consecutive 
blocks (slices) and performs a supervised classification of these blocks 
against a training set. A number of machine-learning methods for 
classification used in computational stylistics are available: Delta, 
k-Nearest Neighbors, Support Vector Machines, Naive Bayes, and Nearest 
Shrunken Centroids.
}
\usage{
rolling.classify(gui = FALSE, training.corpus.dir = "reference_set",
         test.corpus.dir = "test_set", training.frequencies = NULL, 
         test.frequencies = NULL, training.corpus = NULL, 
         test.corpus = NULL,  features = NULL, path = NULL, 
         slice.size = 5000, slice.overlap = 4500, 
         training.set.sampling = "no.sampling", mfw = 100, culling = 0, 
         milestone.points = NULL, milestone.labels = NULL, 
         plot.legend = TRUE, add.ticks = FALSE, shading = FALSE,
         ...)
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical user interface (GUI) will appear. Default value is \code{FALSE}
    so far, since GUI is still under development.}

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

  \item{training.corpus.dir}{the subdirectory (within the current working 
    directory) that contains the training set, or the collection of texts 
    used to exemplify the differences between particular classes (e.g. authors 
    or genres). The discriminating features extracted from this training 
    material will be used during the testing procedure (see below). If not 
    specified, the default subdirectory \code{reference_set} will be used.}

  \item{test.corpus.dir}{the subdirectory (within the working directory) that
    contains a test to be assessed, long enough to be split automatically
    into equal-sized slices, or blocks. If not specified, the default 
    subdirectory \code{test_set} will be used.}

  \item{slice.size}{a text to be analyzed is segmented into consecutive, 
    equal-sized samples (slices, windows, or blocks); the slice size
    is set using this parameter: default is 5,000 words. The samples 
    are allowed to partially overlap (see the next parameter). }

  \item{slice.overlap}{if one specifies a \code{slice.size} of 5,000 
    and a \code{slice.overlap} of 4,500 (which is default), then 
    the first extracted sample contains words 1--5,000, the second 
    501--5,500, the third sample 1001--6,000, and so forth.}
  
  \item{training.set.sampling}{sometimes, it makes sense to split training 
    set texts into smaller samples. Available options: "no.sampling"
    (default), "normal.sampling", "random.sampling". See 
    \code{help(make.samples)} for further details.}
  
  \item{mfw}{number of the most frequent words (MFWs) to be analyzed.}
  
  \item{culling}{culling level; see \code{help(perform.culling)} to get
    some help on the culling procedure principles.}
  
  \item{milestone.points}{sometimes, there is a need to mark one or more 
    passages in an analyzed text (e.g. when external evidence 
    suggests an authorial takeover at a certain point) to compare if 
    the a priori knowledge is confirmed by stylometric evidence.
    To this end, one should add into the test file a string
    "xmilestone" (when input texts are loaded directly from files),
    or specify the break points using this parameter. E.g., to add
    two lines at 10,000 words and 15,000 words, use
    \code{milestone.points = c(10000, 15000)}.}

  \item{milestone.labels}{when milestone points are used (see immediately 
    above), they are automatically labelled using lowercase letters:
    "a", "b", "c" etc. However, one can replace them with custom labels,
    e.g. \code{milestone.labels = c("Act I", "Act II")}.}

  \item{plot.legend}{self-evident. Default: \code{TRUE}.}

  \item{add.ticks}{a graphical parameter: consider adding tiny ticks 
    (short horizontal lines) to see the density of sampling. Default:
  \code{FALSE}.}
  
  \item{shading}{instead of using colors on the final plot, one might 
    choose to use shading hatches, which might be an option to toggle 
    with greyscale, but also with non-black settings thereby allowing 
    for photocopier-friendly charts (even if they may be subjectively 
    unattractive). To use this option, switch it to \code{TRUE}.}
  
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
Eder, M. (2015). Rolling stylometry. "Digital Scholarship in the Humanities", 
    31(3): 457-69.

Eder, M. (2014). Testing rolling stylometry. \url{https://goo.gl/f0YlOR}.
}


\author{Maciej Eder}

\seealso{
\code{\link{classify}}, \code{\link{rolling.delta}}
}

\examples{
\dontrun{
# standard usage (it builds a corpus from a collection of text files):
rolling.classify()

rolling.classify(training.frequencies = "freqs_train.txt",
    test.frequencies = "freqs_test.txt", write.png.file = TRUE,
    classification.method = "nsc")
}
}
%\keyword{classification}
%\keyword{support vector machines}
%\keyword{nearest shrunken centroids}
%\keyword{naive Bayes}
%\keyword{Delta}
%\keyword{k-nearest neighbors}
