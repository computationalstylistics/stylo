\name{classify}
\alias{classify}
\title{Machine-learning classification}
\description{Function for .... It performs Delta (Burrows, 2002), k-Nearest
Neighbors classification, Support Vectors Machines, Naive Bayes, and Nearest
Shrunken Centroids (Jockers and Witten, 2010). Most of the options are derived 
from \code{stylo}
}
\usage{
classify(gui = TRUE, path = "", training.corpus.dir = "primary_set",
test.corpus.dir = "secondary_set")
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{training.corpus.dir}{a subdirectory (within a working directory) that
    contains .....................; if not specified, the subdirectory 
    \code{primary_set} will be used.}

  \item{test.corpus.dir}{a subdirectory (within a working directory) that
    contains .....................; if not specified, the subdirectory 
    \code{secondary_set} will be used.}
}

\details{There are numerous additional options .....................
     ..................... ............... .................
     Refer to ..........}

\value{The function produces ..............................}

\references{
Eder, M. Ketemont, M. and Rybicki, J. (2013). Stylometry with R:
    a suite of tools. In: "Digital Humanities 2013: Conference Abstracts". 
    University of Nebraska-Lincoln, Lincoln, NE, pp. ???-???.

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
