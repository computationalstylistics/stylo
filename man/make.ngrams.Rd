\name{make.ngrams}
\alias{make.ngrams}
\title{Make text n-grams}
\description{Funcion that combines a vector of text units (words, 
  characters, POS-tags, other features) into pairs, triplets, or 
  longer sequences, often referred to as n-grams.
}
\usage{
  make.ngrams(input.text, ngram.size = 1)
}
\arguments{
  \item{input.text}{a vector containing words or characters to be combined
  into n-grams.}
  \item{ngram.size}{an optional argument (integer) indicating the value of n, 
  or the size of n-grams to be produced. If this argument is missing, default
  value of 1 is used.}
}
\details{
  Function for combining single features (words or characters) into n-grams, 
  or strings of n elements. E.g. character 2-grams of the sentence "This is 
  a sentence" are as follows: "th", "hi", "is", "s ", " i", "is", "s ", 
  " a", "a ", " s", "se", "en", "nt", "te", "en", "nc", "ce". Character
  4-grams would be, of course: "this", "his ", "is a", "s a ", " a s", etc.
  Word 2-grams: "this is", "is a", "a sentence".
}
\author{
  Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
  \code{\link{txt.to.words}}, \code{\link{txt.to.words.ext}},  
  \code{\link{txt.to.features}}
}
\examples{
  # assume there is a text:
  my.text = "Quousque tandem abutere, Catilina, patientia nostra?"
  # which can be split into a vector of consecutive words:
  my.vector.of.words = txt.to.words(my.text)
  # now, one can build a vector of word 2-grams:
  make.ngrams(my.vector.of.words, ngram.size = 2)
  # 
  # similarly, one can produce character n-grams:
  my.vector.of.chars = txt.to.features(my.vector.of.words, features = "c")
  make.ngrams(my.vector.of.chars, ngram.size = 4)
}
\keyword{text processing}
