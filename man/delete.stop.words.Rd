\name{delete.stop.words}
\alias{delete.stop.words}
\title{Exclude stop words (e.g. pronouns, particles, etc.) from a dataset}
\description{Function for removing custom words from a dataset: it can be 
        the so-called stop words (frequent words without much meaning), 
        or personal pronouns, or other custom elements of a dataset. It can 
        be used to cull certain words from a vector containing tokenized 
        text (particular words as elements of the vector), or to exclude 
        unwanted columns (variables) from a table with frequencies. See 
        examples below.}

\usage{
delete.stop.words(input.data, stop.words = NULL)
}

\arguments{
  \item{input.data}{either a vector containing words (actually, any countable 
      features), or a data matrix/frame. The former in case of culling 
      stop words from running text, the latter for culling them from tables 
      of frequencies (then particular columns are excluded). The table should 
      be oriented to contain samples in rows, variables in columns, and 
      variables' names should be accessible via \code{colnames(input.table)}.}
      
  \item{stop.words}{a vector of words to be excluded.}
}
\details{
  This function might be usefull to perform culling, or automatic deletion of 
  the words that are too characteristic for particular texts. See 
  \code{help(culling)} for further details.
}
\author{
Maciej Eder
}
\seealso{
  \code{\link{stylo.pronouns}}, \code{\link{perform.culling}}
}
\examples{
# (i) excluding stop words from a vector
my.text = c("omnis", "homines", "qui", "sese", "student", "praestare", 
        "ceteris", "animalibus", "summa", "ope", "niti", "decet", "ne",
        "vitam", "silentio", "transeant", "veluti", "pecora", "quae",
        "natura", "prona", "atque", "ventri", "oboedientia", "finxit")
delete.stop.words(my.text, stop.words = c("qui", "quae", "ne", "atque"))


# (ii) excluding stop words from tabular data
#
# assume there is a matrix containing some frequencies
# (be aware that these counts are fictional):
t1 = c(2, 1, 0, 8, 9, 5, 6, 3, 4, 7)
t2 = c(7, 0, 5, 9, 1, 8, 6, 4, 2, 3)
t3 = c(5, 9, 2, 1, 6, 7, 8, 0, 3, 4)
t4 = c(2, 8, 6, 3, 0, 5, 9, 4, 7, 1)
my.data.table = rbind(t1, t2, t3, t4)

# names of the samples:
rownames(my.data.table) = c("text1", "text2", "text3", "text4")
# names of the variables (e.g. words):
colnames(my.data.table) = c("the", "of", "in", "she", "me", "you",
                                    "them", "if", "they", "he")
# the table looks as follows
print(my.data.table)

# now, one might want to get rid of the words "the", "of", "if":
delete.stop.words(my.data.table, stop.words = c("the", "of", "if"))

# also, some pre-defined lists of pronouns can be applied:
delete.stop.words(my.data.table, 
                      stop.words = stylo.pronouns(corpus.lang = "English"))


}
%\keyword{text processing}
