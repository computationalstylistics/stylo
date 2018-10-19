\encoding{utf8}
\name{change.encoding}
\alias{change.encoding}
\title{
Chance haracter encoding
}
\description{
This function is a wrapper around \code{iconv()} that allow to convert 
character encoing of multiple text files in a corpus folder, preferably 
into UTF-8.
}
\usage{
change.encoding(corpus.dir = "corpus/", from, to = "utf-8", keep.original = TRUE, output.dir = NULL)
}

\arguments{
  \item{corpus.dir}{path to the folder containing the corpus.}
  \item{from}{original character encoding.}
  \item{to}{character encoding to convert into.}
  \item{keep.original}{shall the original files be stored?}
  \item{output.dir}{folder for the reencoded files.}

}
\details{
Stylo works on UTF-8-enconded text by default. This function allows you to convert their corpus, if
not yet encoded in UTF-8. To check the current encoding of text files in your corpus folder, you can
use the function \code{check.encoding()}.
}
\value{
The function saves reencoded text files.
}
\references{
R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical
  Computing, Vienna, Austria. URL https://www.R-project.org/.
}
\author{
Steffen Pielstrom
}
\seealso{
\code{\link{check.encoding}}
}
\examples{\dontrun{
# To replace the old versions with the newly encoded, but retain them in another folder:
change.encoding = function(corpus.dir = "~/corpora/example/", from = "ASCII", to = "utf-8")

# To place the new version in another folder called "utf8/":
change.encoding = function(corpus.dir = "~/corpora/example/",
                           from = "ASCII", 
                           to = "utf-8", 
                           output.dir = "utf8/")
                           
# To simply replace the old version:
change.encoding = function(corpus.dir = "~/corpora/example/", 
                           from = "ASCII", 
                           to = "utf-8",
                           keep.original = FALSE)
}
}