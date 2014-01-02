\name{txt.to.words.ext}
\alias{txt.to.words.ext}
\title{Split text into words}
\description{
Function for splitting a string of characters into single words,
removing punctuation etc., and preserving some language-dependent
idiosyncracies, such as common contractions in English.
}
\usage{
txt.to.words.ext(input.text, language = "English", splitting.rule = NULL)
}
\arguments{
  \item{input.text}{a string of characters, usually a text.}
  \item{language}{an optional argument specifying the language of the texts 
    analyzed. Values that will affect the function's output are: 
    \code{English.contr}, \code{English.all}, \code{Latin.corr} (their 
    meaning is explained below). The default value is \code{English}.}
  \item{splitting.rule}{if you are not satisfied with the default language
    settings (or your input string of characters is not a regular text,
    but a sequence of, say, dance movements represented using symbolic signs),
    you can indicate your custom splitting regular expression here. This
    option will overwrite the above language settings. For further details,
    refer to \code{help(txt.to.words)}.}
}
\details{
Function for splitting a given input text into single words (chains 
of characters delimited with spaces or punctuation marks). It is build on 
top of the function \code{txt.to.words} and it is designed to manage some 
language-dependent text features during the tokenization process. In most
languages, this is irrelevant. However, it might be important when with English
or Latin texts: \code{English.contr} treats contractions as single, atomary words, 
i.e. strings such as "don't", "you've" etc. will not be split into two strings;
\code{English.all} keeps the contractions (as above), and also prevents
the function from splitting compound words (mother-in-law, double-decker, etc.);
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
%\keyword{text processing}
