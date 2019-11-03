\name{txt.to.words.ext}
\alias{txt.to.words.ext}
\title{Split text into words: extended version}
\description{
Function for splitting a string of characters into single words,
removing punctuation etc., and preserving some language-dependent
idiosyncracies, such as common contractions in English.
}
\usage{
txt.to.words.ext(input.text, corpus.lang = "English", splitting.rule = NULL, 
                 preserve.case = FALSE)
}
\arguments{
  \item{input.text}{a string of characters, usually a text.}
  \item{corpus.lang}{an optional argument specifying the language of the texts 
    analyzed. Values that will affect the function's output are: 
    \code{English.contr}, \code{English.all}, \code{Latin.corr} (their 
    meaning is explained below), \code{JCK} for Japanese, Chinese and Korean,
    as well as \code{other} for a variety of non-Latin scripts, including 
    Cyryllic, Greek, Arabic, Hebrew, Coptic, Georgian etc. The default 
    value is \code{English}.}
  \item{splitting.rule}{if you are not satisfied with the default language
    settings (or your input string of characters is not a regular text,
    but a sequence of, say, dance movements represented using symbolic signs),
    you can indicate your custom splitting regular expression here. This
    option will overwrite the above language settings. For further details,
    refer to \code{help(txt.to.words)}.}
  \item{preserve.case}{Whether or not to lowercase all character in the corpus 
    (default = \code{FALSE}).}
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
the function from splitting compound words (mother-in-law, double-decker, etc.).
\code{Latin.corr}: since some editions do not distinguish the letters v/u,
this setting provides a consistent conversion to "u" in the whole string. The
option \code{preserve.case} lets you specify whether you wish to lowercase all
characters in the corpus.
}
\author{
Maciej Eder, Mike Kestemont
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
sample.text = "That's how your money-making is justified here."
txt.to.words.ext(sample.text, corpus.lang = "English")
txt.to.words.ext(sample.text, corpus.lang = "English.contr")
txt.to.words.ext(sample.text, corpus.lang = "English.all")
}
%\keyword{text processing}
