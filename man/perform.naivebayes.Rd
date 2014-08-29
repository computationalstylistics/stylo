\name{perform.naivebayes}
\alias{perform.naivebayes}
\title{Naive Bayes classifier}
\description{
A machine-learning supervised classifier; this function is a wrapper for 
the Naive Bayes procedure provided by the package \code{e1071}.
}
\usage{
perform.naivebayes(training.set, test.set) }
\arguments{
\item{training.set}{a table containing frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of text
          samples (for the training set). Make sure that
          the rows contain samples, and the columns -- variables
          (words, n-grams, or whatever needs to be analyzed).}

\item{test.set}{a table containing frequencies/counts for the training set. 
          The variables used (i.e. columns) must match the columns of the 
          training set.}

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
\code{\link{perform.delta}}, \code{\link{perform.knn}}
}
\examples{
\dontrun{
perform.naivebayes(training.set, test.set)
}
}
%\keyword{classification}
