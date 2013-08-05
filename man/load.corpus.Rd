\name{load.corpus}
\alias{load.corpus}
\title{Load text files}
\description{
Function for loading text files from a specified directory.
}
\usage{
load.corpus(files, corpus.dir = "")
}
\arguments{
  \item{files}{a vector of file names.}
  \item{corpus.dir}{a directory containing the text files to be loaded; if
    not specified, the current working directory will be used.}
}
\value{
The function returns a variable (list), containing as elements the texts loaded.
}

\author{Maciej Eder}

\seealso{
\code{\link{stylo}}, \code{\link{classify}}, \code{\link{rolling.delta}}, 
\code{\link{oppose}} 
}
\examples{
\dontrun{
# to load file1.txt and file2.txt, stored in the subdirectory my.files:
my.corpus = load.corpus(corpus.dir = "my.files",
                        files = c("file1.txt", "file2.txt") )

# to load all XML files from the current directory:
my.corpus = load.corpus(files = list.files(pattern="[.]xml$") )
}
}
%\keyword{text processing}
