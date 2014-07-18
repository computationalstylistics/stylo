\name{delete.stop.words}
\alias{delete.stop.words}
\title{Exclude variables (e.g. pronouns) from a frequency table}
\description{Function for removing custom variables from a table of frequencies;
        it can be the so-called stop words (frequent words without much 
        meaning), or personal pronouns, or other custom elements of a dataset.}

\usage{
delete.stop.words(input.table, stop.words = NULL)
}

\arguments{
  \item{input.table}{a matrix or data frame containing frequencies of words or any other countable features; the table should be oriented to contain samples in rows, variables in columns, and variables' names should be accessible via \code{colnames(input.table)}.}
  \item{stop.words}{a vector of words to be excluded.}
}
\details{
  This function might be usefull to perform culling, or automatic deletion of the words that are too characteristic for particular texts. See \code{help(culling) for further details.}
}
\author{
Maciej Eder
}
\seealso{
  \code{\link{stylo.pronouns}}, \code{\link{culling}}
}
\examples{
        # assume there is a matrix containing some frequencies
        # (be aware that these counts are fictional):
        text1 = c(2, 1, 0, 8, 9, 5, 6, 3, 4, 7)
        text2 = c(7, 0, 5, 9, 1, 8, 6, 4, 2, 3)
        text3 = c(5, 9, 2, 1, 6, 7, 8, 0, 3, 4)
        text4 = c(2, 8, 6, 3, 0, 5, 9, 4, 7, 1)
        my.data.table = rbind(text1, text2, text3, text4)

        # names of the samples:
        rownames(my.data.table) = c("t1", "t2", "t3", "t4")
        # names of the variables (e.g. words):
        colnames(my.data.table) = c("the", "of", "in", "she", "me", "you",
                                    "them", "if", "they", "he")
        # the table looks as follows
        my.data.table
        
        # now, one might want to get rid of the words "the", "of", "if":
        delete.stop.words(my.data.table, stop.words = c("the", "of", "if"))
        
        # also, some pre-defined lists of pronouns can be applied:
        delete.stop.words(my.data.table, 
                          stop.words = stylo.pronouns(language = "English"))


}
%\keyword{text processing}
