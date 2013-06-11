\name{txt.to.features}
\alias{txt.to.features}
\title{Split string of words}
\description{
Function that converts a vector of words into either words, or characters, 
and optionally agglutinates them them into n-grams.
}
\usage{
txt.to.features(tokenized.text, features = "w", ngram.size = 1)
}
\arguments{
  \item{tokenized.text}{a vector of separated words}
  \item{features}{an option for specifying the desired type of feature:
    \code{w} for words, \code{c} for characters (default: \code{w}).}
  \item{ngram.size}{an optional argument (integer) indicating the value of n, 
    or the size of n-grams to be produced. If this argument is missing, 
    default value of 1 is used.}
}
\details{
Function that carries out the necessary modifications for 
feature selection: convert an input text into the type of sequence 
needed (n-grams etc.) and returns a new vector of items. The function
invokes \code{make.ngrams} to combine single units into pairs, 
triplets or longer n-grams; See  \code{help(make.ngrams)} for details.
}
\author{
Maciej Eder, Mike Kestemont
}
\seealso{
\code{\link{txt.to.words}}, \code{\link{txt.to.words.ext}},  
\code{\link{make.ngrams}}
}
\examples{
# assume there is a text:
my.text = "Quousque tandem abutere, Catilina, patientia nostra?"
# which can be split into a vector of consecutive words:
my.vector.of.words = txt.to.words(my.text)
# now, one can build a vector of word 2-grams:
txt.to.features(my.vector.of.words, ngram.size = 2)
# 
# similarly, one can produce character n-grams:
txt.to.features(my.vector.of.words, features = "c", ngram.size = 4)
}
\keyword{text processing}
