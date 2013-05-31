\name{load.corpus.and.parse}
\alias{load.corpus.and.parse}
\title{Load text files and perform pre-processing}
\description{
A high-level function that ties a number of other 
functions responsible for uploading texts, deleting markup, sampling, 
splitting into n-grams, etc. It is build on top of a number of functions 
and thus it requires a large number of arguments. The only required
argument, however, is a vector containing names of files to upload.
}
\usage{load.corpus.and.parse(files, corpus.dir = "", markup.type = "plain",
language = "English", sample.size = 10000, sampling = "no.sampling",
sampling.with.replacement = FALSE, features = "w", ngram.size = 1)
}
\arguments{
  \item{files}{a vector of file names.}
  \item{corpus.dir}{a directory containing text files to be uploaded; if
    not specified, the current directory will be used.}
  \item{markup.type}{choose one of the following values: \code{plain} 
    (nothing will happen), \code{html} (all tags will be deleted as well 
    as HTML header), \code{xml} (TEI header, any text between <note> </note> 
    tags, and all the tags will be deleted), \code{xml.drama} (as above;
    additionally speaker's names will be deleted, or strings within the 
    tag <speaker> </speaker>); see \code{delete.markup} for further details.}
  \item{language}{an optional argument indicating the language; the values
    that will affect the function's behavior are: \code{English.contr},
    \code{English.all}, \code{Latin.corr} (see \code{txt.to.words.ext}
    for explanation). The default value is \code{English}.}
  \item{sample.size}{desired size of sample expressed in number of words;
    default value is 10,000.}
  \item{sampling}{one of three values: \code{no.sampling} (default), 
     \code{normal.sampling}, \code{random.sampling}. See \code{make.samples}
     for explanation.}
  \item{sampling.with.replacement}{optional argument which will be used only
    if \code{random.sampling} was chosen; it indicates the way of colloecting
    random words.}
  \item{features}{an option for specifying the desired type of feature:
    \code{w} for words, \code{c} for characters (default: \code{w}). See
    \code{txt.to.features} for further details.}
  \item{ngram.size}{an optional argument (integer) indicating the value of n, 
    or the size of n-grams to be produced. If this argument is missing, 
    default value of 1 is used. See \code{txt.to.features} for further 
    details.}
}
\value{
The result of this function is a variable (list) \code{loaded.corpus}, 
its elements are particular uploaded texts.
}
\author{Maciej Eder}

\seealso{
\code{\link{load.corpus}}, \code{\link{delete.markup}}, 
\code{\link{txt.to.words.ext}}, \code{\link{txt.to.features}},
\code{\link{make.samples}}
}

\examples{
\dontrun{
#
# to upload file1.txt and file2.txt, stored in subdirectory "my.files":
my.corpus = load.corpus(files = c("file1.txt","file2.txt"),
                        corpus.dir = "my.files")
# to upload all XML files from current directory and get rid of tags:
my.corpus = load.corpus(files = list.files(pattern="[.]xml$"),
                        markup.type="xml")
}
}
\keyword{text processing}
