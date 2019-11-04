\name{make.ngrams}
\alias{make.ngrams}
\title{Make text n-grams}
\description{
Function that combines a vector of text units (words, 
characters, POS-tags, other features) into pairs, triplets, or 
longer sequences, commonly referred to as n-grams.
}
\usage{
make.ngrams(input.text, ngram.size = 1)
}
\arguments{
  \item{input.text}{a vector containing words or characters to be parsed
    into n-grams.}
  \item{ngram.size}{an optional argument (integer) indicating the value 
    of \emph{n}, or the size of n-grams to be produced. If this argument 
    is missing, default value of 1 is used.}
}
\details{
Function for combining series of items (e.g. words or characters) into n-grams, 
or strings of \emph{n} elements. E.g. character 2-grams of the sentence "This is 
a sentence" are as follows: "th", "hi", "is", "s ", " i", "is", "s ", 
" a", "a ", " s", "se", "en", "nt", "te", "en", "nc", "ce". Character
4-grams would be, of course: "this", "his ", "is a", "s a ", " a s", etc.
Word 2-grams: "this is", "is a", "a sentence". The issue whether using 
n-grams of items increases the accuracy of stylometric procedures has been 
heavily debated in the secondary literature (see the reference section 
for further reading). Eder (2013) e.g. shows that character n-grams 
are suprisingly robust for dealing with noisy corpora (in terms of a high 
number of misspelled characters).
}
\references{
Alexis, A., Craig, H., and Elliot, J. (2014). Language chunking, 
    data sparseness, and the value of a long marker list: explorations with 
    word n-grams and authorial attribution. "Literary and Linguistic
    Computing", 29, advanced access (doi: 10.1093/llc/fqt028).

Eder, M. (2011). Style-markers in authorship attribution: a cross-language 
    study of the authorial fingerprint. "Studies in Polish Linguistics", 
    6: 99-114. \url{http://www.ejournals.eu/SPL/2011/SPL-vol-6-2011/}.

Eder, M. (2013). Mind your corpus: systematic errors in authorship
    attribution. "Literary and Linguistic Computing", 28(4): 603-14.

Hoover, D. L. (2002). Frequent word sequences and statistical stylistics.
    "Literary and Linguistic Computing", 17: 157-80.

Hoover, D. L. (2003). Frequent collocations and authorial style.
    "Literary and Linguistic Computing", 18: 261-86.

Hoover, D. L. (2012). The rarer they are, the more they are, the less
    they matter. In: Digital Humanities 2012: Conference Abstracts,
    Hamburg University, Hamburg, pp. 218-21.

Koppel, M., Schler, J. and Argamon, S. (2009). Computational methods
    in authorship attribution. "Journal of the American Society for 
    Information Science and Technology", 60(1): 9-26.

Stamatatos, E. (2009). A survey of modern authorship attribution methods.
    "Journal of the American Society for Information Science and Technology", 
    60(3): 538-56.
}
\author{
Maciej Eder
}
\seealso{
\code{\link{txt.to.words}}, \code{\link{txt.to.words.ext}},  
\code{\link{txt.to.features}}
}
\examples{
# Consider the string my.text:
my.text = "Quousque tandem abutere, Catilina, patientia nostra?"
# which can be split into a vector of consecutive words:
my.vector.of.words = txt.to.words(my.text)
# now, we create a vector of word 2-grams:
make.ngrams(my.vector.of.words, ngram.size = 2)

# similarly, you can produce character n-grams:
my.vector.of.chars = txt.to.features(my.vector.of.words, features = "c")
make.ngrams(my.vector.of.chars, ngram.size = 4)
}
%\keyword{text processing}
