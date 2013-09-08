\name{load.corpus.and.parse}
\alias{load.corpus.and.parse}
\title{Load text files and perform pre-processing}
\description{
A high-level function that controls a number of other 
functions responsible for loading texts, deleting markup, sampling from texts, 
converting samples to n-grams, etc. It is build on top of a number of functions 
and thus it requires a large number of arguments. The only obligatory
argument, however, is a vector containing the names of the files to be loaded.
}
\usage{load.corpus.and.parse(files, corpus.dir = "", markup.type = "plain",
language = "English", sample.size = 10000, sampling = "no.sampling",
sampling.with.replacement = FALSE, features = "w", ngram.size = 1)
}
\arguments{
  \item{files}{a vector of file names.}
  \item{corpus.dir}{the directory containing the text files to be loaded; if
    not specified, the current directory will be used.}
  \item{markup.type}{choose one of the following values: \code{plain} 
    (nothing will happen), \code{html} (all tags will be deleted as well 
    as HTML header), \code{xml} (TEI header, any text between <note> </note> 
    tags, and all the tags will be deleted), \code{xml.drama} (as above;
    additionally, speaker's names will be deleted, or strings within the <speaker> </speaker> tags); 
    see \code{delete.markup} for further details.}
  \item{language}{an optional argument indicating the language of the texts analyzed; the values
    that will affect the function's behavior are: \code{English.contr},
    \code{English.all}, \code{Latin.corr} (see \code{txt.to.words.ext}
    for explanation). The default value is \code{English}.}
  \item{sample.size}{desired size of samples, expressed in number of words;
    default value is 10,000.}
  \item{sampling}{one of three values: \code{no.sampling} (default), 
     \code{normal.sampling}, \code{random.sampling}. See \code{make.samples}
     for explanation.}
  \item{sampling.with.replacement}{optional argument which will be used only
    if \code{random.sampling} was chosen; it specifies the method used to randomly harvest words from texts.}
  \item{features}{an option for specifying the desired type of features:
    \code{w} for words, \code{c} for characters (default: \code{w}). See
    \code{txt.to.features} for further details.}
  \item{ngram.size}{an optional argument (integer) specifying the value of \emph{n}, 
    or the size of n-grams to be produced. If this argument is missing, 
    the default value of 1 is used. See \code{txt.to.features} for further 
    details.}
}
\value{
The result of this function is a variable (list) containing 
as elements the samples (entire texts or sampled subsets) split into 
words/characters and combined into n-grams (if applicable).
}
\author{Maciej Eder}

\seealso{
\code{\link{load.corpus}}, \code{\link{delete.markup}}, 
\code{\link{txt.to.words.ext}}, \code{\link{txt.to.features}},
\code{\link{make.samples}}
}

\examples{
\dontrun{
# to load file1.txt and file2.txt, stored in the subdirectory my.files:
my.corpus = load.corpus.and.parse(files = c("file1.txt", "file2.txt"),
                        corpus.dir = "my.files")

# to load all XML files from the current directory, while getting rid of
# all markup tags in the file, and split the texts into consecutive 
# word pairs (2-grams):
my.corpus = load.corpus.and.parse(files = list.files(pattern = "[.]xml$"),
                        markup.type = "xml", ngram.size = 2)
}
}
%\keyword{text processing}
