\name{load.corpus}
\alias{load.corpus}
\title{Load text files}
\description{
Function for uploading text files from a specified directory.
}
\usage{
load.corpus(files, corpus.dir = "")
}
\arguments{
  \item{files}{a vector of file names.}
  \item{corpus.dir}{a directory containing text files to be uploaded; if
    not specified, a current directory will be used.}
}
\value{
The result of this function is a variable (list) \code{loaded.corpus}, 
its elements are particular uploaded texts.
}

\author{Maciej Eder}

\seealso{
\code{\link{stylo}}, \code{\link{classify}}, \code{\link{rolling.delta}}, 
\code{\link{oppose}} 
}
\examples{
\dontrun{
#
# to upload file1.txt and file2.txt, stored in the subdirectory "my.files":
my.corpus = load.corpus(corpus.dir = "my.files",
                        files = c("file1.txt","file2.txt"))
# to upload all XML files from the current directory:
my.corpus = load.corpus(files = list.files(pattern="[.]xml$"))
}
}
\keyword{text processing}
