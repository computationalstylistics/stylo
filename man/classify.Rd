\name{classify}
\alias{classify}
\title{Machine-learning classification}
\description{Function for .... It performs Delta (Burrows, 2002), k-Nearest
Neighbors classification, Support Vectors Machines, Naive Bayes, and Nearest
Shrunken Centroids (Jockers et al., 2008). Most of the options are derived 
from \code{stylo}}

\usage{
classify(gui = TRUE, path = "", training.corpus.dir = "primary_set",
test.corpus.dir = "secondary_set")
}

\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{primary.corpus.dir}{a subdirectory (within a working directory) that
    contains .....................; if not specified, the subdirectory 
    \code{primary_set} will be used.}

  \item{secondary.corpus.dir}{a subdirectory (within a working directory) that
    contains .....................; if not specified, the subdirectory 
    \code{secondary_set} will be used.}
}

\details{There are numerous additional options .....................
     ..................... ............... .................
     Refer to ..........}

\value{The function produces ..............................}

\references{
  Eder, M. and Rybicki, J. (2011). Stylometry with R. In: 
    \code{Digital Humanities 2011: Conference Abstracts}. Stanford University,
    Stanford, pp. 308-11.

Burrows

Jockers et al.

Argamon

}

\author{Maciej Eder, Mike Kestemont, Jan Rybicki}

\seealso{
 \code{\link{stylo}}, \code{\link{rolling.delta}}, \code{\link{oppose}}}

\examples{
  \dontrun{
  #
  # standard usage:
  classify()
  #
  # batch mode, custom name of corpus directories:
  classify(gui = FALSE, training.corpus.dir = "TrainingSet",
         test.corpus.dir = "TestSet")
  #
  }
}
\keyword{classification}
\keyword{support vector machines}
\keyword{nearest shrunken centroids}
\keyword{naive Bayes}
\keyword{k-NN}
