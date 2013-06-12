\name{classify}
\alias{classify}
\title{Machine-learning classification}
\description{Function that performs a number of machine-learning methods
of classification used in computational stylistics: Delta (Burrows, 2002), 
k-Nearest Neighbors classification, Support Vectors Machines, Naive Bayes, 
and Nearest Shrunken Centroids (Jockers and Witten, 2010). Most of the options 
are derived from the \code{stylo} function.
}
\usage{
classify(gui = TRUE, path = "", training.corpus.dir = "primary_set",
test.corpus.dir = "secondary_set", ...)
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{training.corpus.dir}{a subdirectory (within a working directory) that
    contains the training set, or the collection of texts used to capture 
    the differences between particular classes (e.g. authors or genres); 
    this extracted discriminative features are used in the testing procedure 
    (see below). If not specified, the subdirectory 
    \code{primary_set} will be used.}

  \item{test.corpus.dir}{a subdirectory (within a working directory) that
    contains the test set, or the collection of texts that are used to 
    confirm robustness of the discriminative features as extracted from 
    the training set. If a question of authorship is being approached, 
    this set should contain works written by known authors, to confirm 
    if the procedure really 'recognizes' them. This set contains 'new' 
    data, in terms of anonymous samples or other textual material that 
    one wants to be classified. If not specified, the subdirectory 
    \code{secondary_set} will be used.}

  \item{...}{any variable as produced by \code{stylo.default.settings}
    can be set here, to overwrite the default values.}
}

\details{There are numerous additional options that are passed to 
this function; so far, they are all loaded when stylo.default.settings 
is executed (it will be invoked automatically from inside this function);
the user can set/change them on GUI.}

\value{The function produces a number of files containing final results, 
tables of distances, and so on (depending on which options have been chosen).}

\references{
Eder, M. Ketemont, M. and Rybicki, J. (2013). Stylometry with R:
    a suite of tools. In: "Digital Humanities 2013: Conference Abstracts". 
    University of Nebraska-Lincoln, Lincoln, NE (in press).

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
#
# standard usage:
classify()
#
# batch mode, custom name of corpus directories:
classify(gui = FALSE, training.corpus.dir = "TrainingSet",
       test.corpus.dir = "TestSet")
}
}
\keyword{classification}
\keyword{support vector machines}
\keyword{nearest shrunken centroids}
\keyword{naive Bayes}
\keyword{k-NN}
