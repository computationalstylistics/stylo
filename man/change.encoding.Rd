\encoding{utf8}
\name{change.encoding}
\alias{change.encoding}
\title{
Change character encoding
}
\description{
This function is a wrapper around \code{iconv()} that allows for converting 
character encoding of multiple text files in a corpus folder, preferably 
into UTF-8.
}
\usage{
change.encoding(corpus.dir = "corpus/", from, to = "utf-8", 
                keep.original = TRUE, output.dir = NULL)
}

\arguments{
  \item{corpus.dir}{path to the folder containing the corpus.}
  \item{from}{original character encoding. See the Details section (below) for some hints on how to get the original encoding.}
  \item{to}{character encoding to convert into.}
  \item{keep.original}{shall the original files be stored?}
  \item{output.dir}{folder for the reencoded files.}

}
\details{
Stylo works on UTF-8-enconded texts by default. This function allows you to convert your corpus, if not yet encoded in UTF-8. To check the current encoding of text files in your corpus folder, you can use the function \code{check.encoding()}.
}
\value{
The function saves reencoded text files.
}
\author{
Steffen Pielström
}
\seealso{
\code{\link{check.encoding}}
}
\examples{\dontrun{
# To replace the old versions with the newly encoded, but retain them 
# in another folder:
change.encoding = function(corpus.dir = "~/corpora/example/", 
                           from = "ASCII", to = "utf-8")

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