\name{perform.culling}
\alias{perform.culling}
\title{Exclude variables (e.g. words, n-grams) from a frequency table that are too characteristic for some samples}
\description{Culling refers to the automatic manipulation of the wordlist (proposed by Hoover 2004a, 2004b). The culling values specify the degree to which words that do not appear in all the texts of a corpus will be removed. A culling value of 20 indicates that words that appear in at least 20\% of the texts in the corpus will be considered in the analysis. A culling setting of 0 means that no words will be removed; a culling setting of 100 means that only those words will be used in the analysis that appear in all texts of the corpus at least once.}

\usage{
perform.culling(input.table, culling.level = 0)
}

\arguments{
  \item{input.table}{a matrix or data frame containing frequencies of words or any other countable features; the table should be oriented to contain samples in rows, variables in columns, and variables' names should be accessible via \code{colnames(input.table)}.}
  \item{culling.level}{percentage of samples that need to have a given word in order to prevent this word from being culled (see the description above).}
}
\references{   
Hoover, D. (2004a). Testing Burrows's Delta. "Literary and Linguistic 
    Computing", 19(4): 453-75.
    
Hoover, D. (2004b). Delta prime. "Literary and Linguistic Computing",
    19(4): 477-95.
}
\author{
Maciej Eder
}
\seealso{
  \code{\link{delete.stop.words}}, \code{\link{stylo.pronouns}}
}
\examples{
# assume there is a matrix containing some frequencies
# (be aware that these counts are entirely fictional):
t1 = c(2, 1, 0, 2, 9, 1, 0, 0, 2, 0)
t2 = c(1, 0, 4, 2, 1, 0, 3, 0, 1, 3)
t3 = c(5, 2, 2, 0, 6, 0, 1, 0, 0, 0)
t4 = c(1, 4, 1, 0, 0, 0, 0, 3, 0, 1)
my.data.table = rbind(t1, t2, t3, t4)

# names of the samples:
rownames(my.data.table) = c("text1", "text2", "text3", "text4")
# names of the variables (e.g. words):
colnames(my.data.table) = c("the", "of", "in", "she", "me", "you",
                                    "them", "if", "they", "he")
# the table looks as follows
print(my.data.table)

# selecting the words that appeared in at laest 50% of samples:
perform.culling(my.data.table, 50)        

}
%\keyword{text processing}
