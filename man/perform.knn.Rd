\name{perform.knn}
\alias{perform.knn}
\title{k-Nearest Neighbor classifier}
\description{
A machine-learning supervised classifier; this function is a wrapper for 
the k-NN procedure provided by the package \code{class}.
}
\usage{
perform.knn(training.set, test.set, k.value = 1) }
\arguments{
\item{training.set}{a table containing frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of text
          samples (for the training set). Make sure that
          the rows contain samples, and the columns -- variables
          (words, n-grams, or whatever needs to be analyzed).}

\item{test.set}{a table containing frequencies/counts for the training set. 
          The variables used (i.e. columns) must match the columns of the 
          training set.}

\item{k.value}{number of nearest neighbors considered.}

}
\value{
The function returns a vector of "guessed" classes: each test sample is
linked with one of the classes represented in the training set. 
}
\author{
Maciej Eder
}
\seealso{
\code{\link{perform.svm}}, \code{\link{perform.nsc}},  
\code{\link{perform.delta}}, \code{\link{perform.naivebayes}}
}
\examples{
\dontrun{
perform.knn(training.set, test.set)
}
}
%\keyword{classification}
