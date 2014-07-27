\name{txt.to.features}
\alias{txt.to.features}
\title{Split string of words or other countable features}
\description{
Function that converts a vector of words into either words, or characters, 
and optionally parses them into n-grams.
}
\usage{
txt.to.features(tokenized.text, features = "w", ngram.size = 1)
}
\arguments{
  \item{tokenized.text}{a vector of tokinzed words}
  \item{features}{an option for specifying the desired type of feature:
    \code{w} for words, \code{c} for characters (default: \code{w}).}
  \item{ngram.size}{an optional argument (integer) indicating the value of \emph{n}, 
    or the size of n-grams to be created. If this argument is missing, 
    the default value of 1 is used.}
}
\details{
Function that carries out the preprocessing steps necessary for 
feature selection: converts an input text into the type of sequences 
needed (n-grams etc.) and returns a new vector of items. The function
invokes \code{make.ngrams} to combine single units into pairs, 
triplets or longer n-grams. See \code{help(make.ngrams)} for details.
}
\author{
Maciej Eder, Mike Kestemont
}
\seealso{
\code{\link{txt.to.words}}, \code{\link{txt.to.words.ext}},  
\code{\link{make.ngrams}}
}
\examples{
# consider the string my.text:
my.text = "Quousque tandem abutere, Catilina, patientia nostra?"

# split it into a vector of consecutive words:
my.vector.of.words = txt.to.words(my.text)

# build a vector of word 2-grams:
txt.to.features(my.vector.of.words, ngram.size = 2)
 
# or produce character n-grams (in this case, character tetragrams):
txt.to.features(my.vector.of.words, features = "c", ngram.size = 4)
}
%\keyword{text processing}
