\name{make.table.of.frequencies}
\alias{make.table.of.frequencies}
\title{Prepare a table of word frequencies}
\description{Function that collects a bunch of frequency lists and puts 
them all in one table of frequencies; certainly, it means performing a number
of rearrangements inside particular lists in order to bind them in parallel.
The table is produced using a reference list of words (passed as an argument).
}
\usage{
make.table.of.frequencies(corpus, words, absent.sensitive = TRUE)
}
\arguments{
\item{corpus}{textual data: either a corpus (stored in a list), or 
a single text (vector).}
\item{words}{a vector containig a reference word list that will be used
to build a table of frequencies.}
\item{absent.sensitive}{this optional argument is used to prevent building 
tables of non-existing words. When switched on (default), the variables 
that contain but O values in all samples, will be excluded. Hovewer, in some 
cases this is important to keep all the variables regardless of their values. 
It is the case of comparing two corpora: even if a given word did not occur 
in the whole corpus A, it might have occured in the corpus B. In short: 
if you perform any analysis involving I and II set, switch this option to 
\code{FALSE}.}
}

\author{Maciej Eder, Jan Rybicki, Mike Kestemont}

\seealso{
\code{\link{load.corpus}}, \code{\link{load.corpus.and.parse}}
}

\examples{
  \dontrun{
  #
  # to upload file1.txt and file2.txt, stored in subdirectory "my.files":
  my.corpus = load.corpus(corpus.dir = "my.files",
                          files = c("file1.txt","file2.txt"))
  # to upload all XML files from current directory:
  my.corpus = load.corpus(files = list.files(pattern="[.]xml$"))
  #
  }
}


\keyword{text processing}
