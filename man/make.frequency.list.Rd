\name{make.frequency.list}
\alias{make.frequency.list}
\title{Make List of the Most Frequent Elements (e.g. Words)}
\description{
Function for generating a frequency list of words or other (linguistic)
features. It basically counts the elements of a vector and returns a vector
of these elements in descending order of frequency.
}
\usage{
make.frequency.list(data, value = FALSE, head = NULL, relative = TRUE)
}
\arguments{
\item{data}{either a vector of elements (e.g. words, letter n-grams),
          or an object of a class \code{stylo.corpus} as produced by the 
          function \code{load.corpus.and.parse}.}

\item{value}{if this function is switched on, not only the most frequent
          elements are returned, but also their frequencies. Default: FALSE.}

\item{head}{this option is meant to limit the number of the most frequent
          features to be returned. Default value is NULL, which means
          that the entire range of frequent and unfrequent features is 
          returned.}

\item{relative}{if you've switched on the option \code{value} (see above),
          you might want to convert your frequencies into relative frequencies,
          i.e. the counted occurrences divided by the length of the input 
          vector -- in a vast majority of cases you should use it, in order 
          to neutralize different sample sizes. Default: TRUE.}

}
\value{
The function returns a vector of features (usually, words) in a descending
order of their frequency. Alternatively, when the option \code{value} is set
TRUE, it returns a vector of frequencies instead, and the features themselves
might be accessed using the generic \code{names} function.
}
\author{
Maciej Eder
}
\seealso{
\code{\link{load.corpus.and.parse}}, \code{\link{make.table.of.frequencies}}
}
\examples{
# assume there is a text:
text = "Mr. Sherlock Holmes, who was usually very late in the mornings, 
       save upon those not infrequent occasions when he was up all night, 
       was seated at the breakfast table. I stood upon the hearth-rug and 
       picked up the stick which our visitor had left behind him the night 
       before. It was a fine, thick piece of wood, bulbous-headed, of the 
       sort which is known as a \"Penang lawyer.\""

# this text can be converted into vector of words:
words = txt.to.words(text)

# an avanced tokenizer is available via the function 'txt.to.words.ext':
words2 = txt.to.words.ext(text, corpus.lang = "English.all")

# a frequency list (just words):
make.frequency.list(words)
make.frequency.list(words2)

# a frequency list with the numeric values
make.frequency.list(words2, value = TRUE)

\dontrun{
# #####################################
# using the function with large text collections

# first, load and pre-process a corpus from 3 text files:
dataset = load.corpus.and.parse(files = c("1.txt", "2.txt", "3.txt"))
#       
# then, return 100 the most frequent words of the entire corpus:
make.frequency.list(dataset, head = 100)
}
}
%\keyword{text processing}
