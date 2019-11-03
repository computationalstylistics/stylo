\name{load.corpus}
\alias{load.corpus}
\title{Load text files}
\description{
Function for loading text files from a specified directory.
}
\usage{
load.corpus(files = "all", corpus.dir = "", encoding = "UTF-8")
}
\arguments{
  \item{files}{a vector of file names. The default value \code{all} is an 
    equivalent to \code{list.files()}.}
  \item{corpus.dir}{a directory containing the text files to be loaded; if
    not specified, the current working directory will be used.}
  \item{encoding}{useful if you use Windows and non-ASCII alphabets: French,
    Polish, Hebrew, etc. In such a situation, it is quite convenient to 
    convert your text files into Unicode and to set this option to
    \code{encoding = "UTF-8"}. In Linux and Mac, you are always expected
    to use Unicode, thus you don't need to set anything.}
}
\value{
The function returns an object of the class \code{stylo.corpus}. It is a list
containing as elements the texts loaded.
}

\author{Maciej Eder}

\seealso{
\code{\link{stylo}}, \code{\link{classify}}, \code{\link{rolling.classify}}, 
\code{\link{oppose}}, \code{\link{txt.to.words}}
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
