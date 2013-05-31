\name{make.samples}
\alias{make.samples}
\title{Split text to samples}
\description{Function that either splits the input text (a vector of words,
word n-grams, character n-grams, etc.) into equal-sized samples of a desired 
length (expressed in words), or excerpts randomly a number of words from the
original textual data.
}
\usage{
make.samples(tokenized.input.data, sample.size = 10000, 
             sampling = "no.sampling", sampling.with.replacement = FALSE)
}
\arguments{
  \item{tokenized.input.data}{vector of words.}
  \item{sample.size}{desired size of sample expressed in number of words;
    default value is 10,000.}
  \item{sampling}{one of three values: \code{no.sampling} (default), 
     \code{normal.sampling}, \code{random.sampling}.}
  \item{sampling.with.replacement}{optional argument which will be used only
    if \code{random.sampling} was chosen; it indicates the way of random
    collecting words.}
}
\details{
[a few words about normal sampling. Advantages of slicing a text into samples
are discussed by .... (Karina? Mike?)].
When the analyzed texts are significantly unequal in length, it is not 
a bad idea to prepare samples as randomly chosen "bags of words". For this, 
set the \code{sampling} variable to \code{random.sampling}. The desired 
size of the sample should be indicated via the \code{sample.size} variable.
Sampling with and without replacement is also available. It has been shown
(Eder, 2010) that harvesting random samples from original texts improves
the performance of authorhip attribution methods.
}
\references{
Eder, M. (2010). Does size matter? Authorship attribution, short samples, 
    big problem. In "Digital Humanities 2010: Conference Abstracts". 
    King's College London, pp. 132-35.

[anything about normal sampling, Mike?]
}
\author{
Mike Kestemont
}
\seealso{
\code{\link{txt.to.words}}, \code{\link{txt.to.words.ext}},
\code{\link{txt.to.features}},  \code{\link{make.ngrams}}
}
\examples{
my.text = "Arma virumque cano, Troiae qui primus ab oris
           Italiam fato profugus Laviniaque venit
           litora, multum ille et terris iactatus et alto
           vi superum, saevae memorem Iunonis ob iram,
           multa quoque et bello passus, dum conderet urbem
           inferretque deos Latio; genus unde Latinum
           Albanique patres atque altae moenia Romae.
           Musa, mihi causas memora, quo numine laeso
           quidve dolens regina deum tot volvere casus
           insignem pietate virum, tot adire labores
           impulerit. tantaene animis caelestibus irae?"
my.words = txt.to.words(my.text)
make.samples(my.words, sampling = "normal.sampling", sample.size = 20)
#
make.samples(my.words, sampling = "random.sampling", sample.size = 50)
}
\keyword{text processing}
