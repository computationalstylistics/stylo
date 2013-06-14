\name{txt.to.words.ext}
\alias{txt.to.words.ext}
\title{Split text into words}
\description{
Function for splitting a string of characters into single words,
getting rid of punctuation etc., and preserving some language-dependent
idiosyncrasies, such as English contractions.
}
\usage{
txt.to.words.ext(input.text, language = "English")
}
\arguments{
  \item{input.text}{a string of characters, presumably a written text.}
  \item{language}{an optional argument indicating the language; the values
    that will affect the function's behavior are: \code{English.contr},
    \code{English.all}, \code{Latin.corr} (their meaning is explained below).
    The default value is \code{English}.}
}
\details{
Function for splitting a given input text into single words (chains 
of characters delimited with spaces or punctuation marks). It is build on 
top of the function \code{txt.to.words} and it is designed to manage some 
language-dependent text features during the process of splitting. In most
languages, this is irrelevant. However, it might be important for English
and Latin: \code{English.contr} treats the contractions as single words, 
i.e. strings such as "don't", "you've" etc. will not be split into two words;
\code{English.all} keeps the contractions (as above), and also prevents
from splitting compound words (mother-in-law, double-decker, etc.);
\code{Latin.corr} since some editions do not distinguish the letters v/u,
this setting provides a consistent conversion to "u" in the whole string.
}
\author{
Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
\code{\link{txt.to.words}}, \code{\link{txt.to.features}},  
\code{\link{make.ngrams}}
}
\examples{
txt.to.words.ext("Nel mezzo del cammin di nostra vita / mi ritrovai per 
    una selva oscura, che la diritta via era smarrita.")

# to see the difference between particular options for English,
# consider the following sentence from Joseph Conrad's "Nostromo":
txt.to.words.ext("That's how your money-making is justified here.")
txt.to.words.ext("That's how your money-making is justified here.",
      language = "English.contr")
txt.to.words.ext("That's how your money-making is justified here.", 
      language = "English.all")
}
\keyword{text processing}
