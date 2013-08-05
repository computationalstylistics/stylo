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
classify(gui = TRUE, path = "", training.corpus.dir = "primary_set",
test.corpus.dir = "secondary_set", ...)
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical user interface (GUI) will appear. Default value is \code{TRUE}.}

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

  \item{...}{any variable as produced by \code{stylo.default.settings}
    can be set here to overwrite the default values.}
}

\details{There are numerous additional options that are passed to 
this function; so far, they are all loaded when stylo.default.settings 
is executed (it will be invoked automatically from inside this function);
the user can set/change them in the GUI.}

\value{
The function returns a list of variables, including tables of word frequencies, vector of features used, a distance table and some more stuff. Additionally, depending on which options have been chosen, the function produces a number of files containing results, features used, tables of distances, etc.
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
# standard usage:
classify()

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
