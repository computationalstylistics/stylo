\name{txt.to.words}
\alias{txt.to.words}
\title{Split text into words}
\description{
Generic tokenization function for splitting a given input text into single words 
(chains of characters delimited by spaces or punctuation marks).
}
\usage{
txt.to.words(input.text, splitting.rule = NULL, preserve.case = FALSE)
}
\arguments{
  \item{input.text}{a string of characters, usually a text.}
  \item{splitting.rule}{an optional argument indicating an alternative 
  splitting regexp. E.g., if your corpus contains no punctuation, you can 
  use a very simple splitting sequence: \code{"[ \\t\\n]+"} or 
  \code{"[[:space:]]+"} (in this case, any whitespace is assumed to be 
  a word delimiter). If you deal with non-latin scripts, you can indicate 
  your letter characters explicitly: for Hebrew try 
  \code{"[^\U0590-\U05F4]+"}, for most Cyrillic 
  scripts try \code{"[^\u0400-\u0482\u048A\u04FF]+"}. Remember, however, 
  that your texts need to be properly loaded into R (which is quite tricky 
  on Windows; see below).}
  \item{preserve.case}{Whether or not to lowercase all characters in the corpus.}
}
\details{
The generic tokenization function for splitting a given input text into 
single words (chains of characters delimited with spaces or punctuation marks).
The default splitting sequence of chars is \code{"[^[:alpha:]]+"} 
on Mac/Linux, and \code{"\\\\W+_"} on Windows (for an unknown reason, 
regular expressions are not entirely platform-independent).
Alternatively, different tokenization rules can be applied through
the option \code{splitting.rule} (see above). ATTENTION: this is the only 
piece of coding in the library \code{stylo} that might depend on the 
operating system used. While on Mac/Linux the native encoding is Unicode,
on Windows you never know if your text will be loaded proprely. A considerable
solution for Windows users is to convert your texts into Unicode (a variety 
of freeware converters are available on the internet), and to set
an appropriate global option in R: whenever you start a fresh R session, type
\code{options(encoding="UTF-8")} in addition to your \code{library(stylo)}
command. The option \code{preserve.case} lets you specify whether you wish 
to lowercase all characters in the corpus (default = F).
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

# retrieving grammatical codes (POS tags) from a tagged text:
tagged.text = "The_DT family_NN of_IN Dashwood_NNP had_VBD long_RB 
               been_VBN settled_VBN in_IN Sussex_NNP ._."
txt.to.words(tagged.text, splitting.rule = "([A-Za-z,.;!]+_)|[ \n\t]")
}
%\keyword{text processing}
