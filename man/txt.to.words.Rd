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
  a word delimiter). If you deal with non-latin scripts, especially with 
  those that are not supported by the \code{stylo} package yet (e.g. Chinese,
  Japanese, Vietnamese, Georgian), you can indicate your letter characters 
  explicitly: for most Cyrillic scripts try the following code
  \code{"[^\u0400-\u0482\u048A\u04FF]+"}. Remember, however, 
  that your texts need to be properly loaded into R (which is quite tricky 
  on Windows; see below).}
  \item{preserve.case}{Whether or not to lowercase all characters 
  in the corpus (default = F).}
}
\details{
The generic tokenization function for splitting a given input text into 
single words (chains of characters delimited with spaces or punctuation marks).
In obsolete versions of the package \code{stylo}, the default splitting 
sequence of chars was \code{"[^[:alpha:]]+"} on Mac/Linux, and 
\code{"\\\\W+_"} on Windows. Two different splitting rules were used, because
regular expressions are not entirely platform-independent; type 
\code{help(regexp)} for more details. For the sake of compatibility, then, 
in the version 0.5.6 a lengthy list of dozens of letters in a few  alphabets 
(Latin, Cyrillic, Greek, Hebrew so far) has been indicated explicitly:

\preformatted{paste("[^A-Za-z",
    # Latin supplement (Western):
    "\U00C0-\U00FF",
    # Latin supplement (Eastern):
    "\U0100-\U01BF",
    # Latin extended (phonetic):
    "\U01C4-\U02AF",
    # modern Greek:
    "\U0386\U0388-\U03FF",
    # Cyrillic:
    "\U0400-\U0481\U048A-\U0527",
    # Hebrew:
    "\U05C6\U05D0-\U05EA\U05F0-\U05F2",
    # extended Latin:
    "\U1E00-\U1EFF",
    # ancient Greek:
    "\U1F00-\U1FBC\U1FC2-\U1FCC\U1FD0-\U1FDB\U1FE0-\U1FEC\U1FF2-\U1FFC",
    "]+",
    sep="")}

Alternatively, different tokenization rules can be applied through
the option \code{splitting.rule} (see above). ATTENTION: this is the only 
piece of coding in the library \code{stylo} that might depend on the 
operating system used. While on Mac/Linux the native encoding is Unicode,
on Windows you never know if your text will be loaded proprely. A considerable
solution for Windows users is to convert your texts into Unicode (a variety 
of freeware converters are available on the internet), and to use an
appropriate encoding option when loading the files:
\code{read.table("file.txt", encoding = "UTF-8"} or
\code{scan("file.txt", what = "char", encoding = "UTF-8"}. If you use 
the functions provided by the library \code{stylo}, you should pass this 
option as an argument to your chosen function: 
\code{stylo(encoding = "UTF-8")},
\code{classify(encoding = "UTF-8")}.
}
\value{
The function returns a vector of tokenized words (or other units) as elements.
}
\author{
Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
\code{\link{txt.to.words.ext}}, \code{\link{txt.to.features}},  
\code{\link{make.ngrams}}, \code{\link{load.corpus}}
}
\examples{
txt.to.words("And now, Laertes, what's the news with you?")

# retrieving grammatical codes (POS tags) from a tagged text:
tagged.text = "The_DT family_NN of_IN Dashwood_NNP had_VBD long_RB 
               been_VBN settled_VBN in_IN Sussex_NNP ._."
txt.to.words(tagged.text, splitting.rule = "([A-Za-z,.;!]+_)|[ \n\t]")
}
%\keyword{text processing}
