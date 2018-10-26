\name{make.samples}
\alias{make.samples}
\title{Split text to samples}
\description{Function that either splits an input text (a vector of linguistic 
        items, such as words, word n-grams, character n-grams, etc.) into 
        equal-sized samples of a desired length (expressed in words), 
        or excerpts randomly a number of words from the original text.
}
\usage{
make.samples(tokenized.text, sample.size = 10000, 
             sampling = "no.sampling", sample.overlap = 0,
             number.of.samples = 1, sampling.with.replacement = FALSE)
}
\arguments{
  \item{tokenized.text}{input textual data stored either in a form of vector
          (single text), or as a list of vectors (whole corpus); 
          particular vectors should contain tokenized data, i.e. words, 
          word n-grams, or other features, as elements.}
  \item{sample.size}{desired size of sample expressed in number of words;
     default value is 10,000.}
  \item{sampling}{one of three values: \code{no.sampling} (default), 
     \code{normal.sampling}, \code{random.sampling}.}
  \item{sample.overlap}{if this opion is used, a reference text is segmented 
     into consecutive, equal-sized samples that are allowed to partially 
     overlap. If one specifies the \code{sample.size} parameter of 5,000 and 
     the \code{sample.overlap} of 1,000, for example, the first sample of a text 
     contains words 1--5,000, the second 4001--9,000, the third sample 8001--13,000, 
     and so forth.}
  \item{number.of.samples}{optional argument which will be used only if 
     \code{random.sampling} was chosen; it is self-evident.}
  \item{sampling.with.replacement}{optional argument which will be used only 
     if \code{random.sampling} was chosen; it specifies the method to randomly 
     harvest words from texts.}
}
\details{
Normal sampling is probably a good choice when the input texts are 
long: the advantage is that one gets a bigger number of samples which,
in a way, validate the results (when several independent samples excerpted
from one text are clustered together).
When the analyzed texts are significantly unequal in length, it is not 
a bad idea to prepare samples as randomly chosen "bags of words". For this, 
set the \code{sampling} variable to \code{random.sampling}. The desired 
size of the sample should be specified via the \code{sample.size} variable.
Sampling with and without replacement is also available. It has been shown
by Eder (2010) that harvesting random samples from original texts improves
the performance of authorship attribution methods.
}
\references{
Eder, M. (2015). Does size matter? Authorship attribution, small samples, 
    big problem. "Digital Scholarship in the Humanities", 30(2): 167-182.
}
\author{
Mike Kestemont, Maciej Eder
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

# split the above text into samples of 20 words:
make.samples(my.words, sampling = "normal.sampling", sample.size = 20)

# excerpt randomly 50 words from the above text:
make.samples(my.words, sampling = "random.sampling", sample.size = 50)

# excerpt 5 random samples from the above text:
make.samples(my.words, sampling = "random.sampling", sample.size = 50,
             number.of.samples = 5)
}
%\keyword{text processing}
