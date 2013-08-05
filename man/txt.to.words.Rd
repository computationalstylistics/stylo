\name{txt.to.words}
\alias{txt.to.words}
\title{Split text into words}
\description{
Generic tokenization function for splitting a given input text into single words 
(chains of characters delimited by spaces or punctuation marks).
}
\usage{
txt.to.words(input.text)
}
\arguments{
  \item{input.text}{a string of characters or text.}
}
\details{
The generic tokenization function for splitting a given input text into single words 
(chains of characters delimited with spaces or punctuation marks).
Alternatively, the function can be another tokenization rules.
ATTENTION: this is the only piece of coding in this script collection that 
depends on the operating system used.
}
\author{
Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
\code{\link{txt.to.words.ext}}, \code{\link{txt.to.features}},  
\code{\link{make.ngrams}}
}
\examples{
txt.to.words("And now, Laertes, what's the news with you?")
}
%\keyword{text processing}
