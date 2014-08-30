\name{perform.nsc}
\alias{perform.nsc}
\title{Nearest Shrunken Centroids classifier}
\description{
A machine-learning supervised classifier; this function is a wrapper for 
the Nearest Shrunken Centroids procedure provided by the package \code{pamr}.
}
\usage{
perform.nsc(training.set, 
              test.set, 
              classes.training.set = NULL, 
              classes.test.set = NULL, 
              no.of.candidates = 3) }
\arguments{
\item{training.set}{a table containing frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of text
          samples (for the training set). Make sure that
          the rows contain samples, and the columns -- variables
          (words, n-grams, or whatever needs to be analyzed).}

\item{test.set}{a table containing frequencies/counts for the training set. 
          The variables used (i.e. columns) must match the columns of the 
          training set.}

\item{classes.training.set}{a vector containing class identifiers for the
          training set. When missing, the row names of the training set
          table will be used; the assumed classes are the strings of 
          characters followed by the first underscore. Consider the following 
          examples: c("Sterne_Tristram",
          "Sterne_Sentimental", "Fielding_Tom", ...), where the classes
          are the authors' names, and c("M_Joyce_Dubliners",
          "F_Woolf_Night_and_day", "M_Conrad_Lord_Jim", ...), where the
          classes are M(ale) and F(emale) according to authors' gender.
          Note that only the part up to the first underscore in the
          sample's name will be included in the class label.}

\item{classes.test.set}{a vector containing class identifiers for the
          test set. When missing, the row names of the test set
          table will be used (see above).}

\item{no.of.candidates}{how many nearest neighbors will be computed for
          each test sample (default = 3).}

}
\value{
The function returns a vector of "guessed" classes: each test sample is
linked with one of the classes represented in the training set. Additionally,
final scores and final rankings of candidates are returned as attributes.
}
\author{
Maciej Eder
}
\seealso{
\code{\link{perform.delta}}, \code{\link{perform.svm}},  
\code{\link{perform.knn}}, \code{\link{perform.naivebayes}}
}
\examples{
\dontrun{
perform.nsc(training.set, test.set)
}
}
%\keyword{classification}
