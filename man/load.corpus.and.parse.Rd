\name{load.corpus.and.parse}
\alias{load.corpus.and.parse}
\title{Load text files and perform pre-processing}
\description{
A high-level function that controls a number of other functions responsible 
for loading texts from files, deleting markup, sampling from texts, 
converting samples to n-grams, etc. It is build on top of a number of functions 
and thus it requires a large number of arguments. The only obligatory
argument, however, is a vector containing the names of the files to be loaded.
}
\usage{
load.corpus.and.parse(files = "all", corpus.dir = "", markup.type= "plain",
                      corpus.lang = "English", splitting.rule = NULL,
                      sample.size = 10000, sampling = "no.sampling",
                      sample.overlap = 0, number.of.samples = 1,
                      sampling.with.replacement = FALSE, features = "w", 
                      ngram.size = 1, preserve.case = FALSE,
                      encoding = "UTF-8")
}
\arguments{
  \item{files}{a vector of file names. The default value \code{all} is an 
    equivalent to \code{list.files()}.}
  \item{corpus.dir}{the directory containing the text files to be loaded; if
    not specified, the current directory will be used.}
  \item{markup.type}{choose one of the following values: \code{plain} 
    (nothing will happen), \code{html} (all tags will be deleted as well 
    as HTML header), \code{xml} (TEI header, any text between <note> </note> 
    tags, and all the tags will be deleted), \code{xml.drama} (as above;
    additionally, speaker's names will be deleted, or strings within the 
    <speaker> </speaker> tags), \code{xml.notitles} (as above; but, 
    additionally, all the chapter/section (sub)titles will be deleted, 
    or strings within each the <head> </head> tags); 
    see \code{delete.markup} for further details.}
  \item{corpus.lang}{an optional argument indicating the language of the texts 
    analyzed; the values that will affect the function's behavior are: 
    \code{English.contr}, \code{English.all}, \code{Latin.corr} (type 
    \code{help(txt.to.words.ext)} for explanation). The default value 
    is \code{English}.}
  \item{splitting.rule}{if you are not satisfied with the default language
    settings (or your input string of characters is not a regular text,
    but a sequence of, say, dance movements represented using symbolic signs),
    you can indicate your custom splitting regular expression here. This
    option will overwrite the above language settings. For further details,
    refer to \code{help(txt.to.words)}.}
  \item{sample.size}{desired size of samples, expressed in number of words;
    default value is 10,000.}
  \item{sampling}{one of three values: \code{no.sampling} (default), 
     \code{normal.sampling}, \code{random.sampling}. See \code{make.samples}
     for explanation.}
  \item{sample.overlap}{if this opion is used, a reference text is segmented 
     into consecutive, equal-sized samples that are allowed to partially 
     overlap. If one specifies the \code{sample.size} parameter of 5,000 and 
     the \code{sample.overlap} of 1,000, for example, the first sample of a text 
     contains words 1--5,000, the second 4001--9,000, the third sample 8001--13,000, 
     and so forth.}
  \item{number.of.samples}{optional argument which will be used only if 
     \code{random.sampling} was chosen; it is self-evident.}
  \item{sampling.with.replacement}{optional argument which will be used only
    if \code{random.sampling} was chosen; it specifies the method used to 
    randomly harvest words from texts.}
  \item{features}{an option for specifying the desired type of features:
    \code{w} for words, \code{c} for characters (default: \code{w}). See
    \code{txt.to.features} for further details.}
  \item{ngram.size}{an optional argument (integer) specifying the value of \emph{n}, 
    or the size of n-grams to be produced. If this argument is missing, 
    the default value of 1 is used. See \code{txt.to.features} for further 
    details.}
  \item{preserve.case}{whether ot not to lowercase all characters in the corpus 
  (default = F).}
  \item{encoding}{useful if you use Windows and non-ASCII alphabets: French,
    Polish, Hebrew, etc. In such a situation, it is quite convenient to 
    convert your text files into Unicode and to set this option to
    \code{encoding = "UTF-8"}. In Linux and Mac, you are always expected
    to use Unicode, thus you don't need to set anything. In Windows, 
    consider using UTF-8 but don't forget about the way of analyzing native 
    ANSI encoded files: set this option to \code{encoding = "native.enc"}.}
}
\value{
The function returns an object of the class \code{stylo.corpus}. It is a list
containing as elements the samples (entire texts or sampled subsets) split into 
words/characters and combined into n-grams (if applicable).
}
\author{Maciej Eder}

\seealso{
\code{\link{load.corpus}}, \code{\link{delete.markup}}, 
\code{\link{txt.to.words}}, \code{\link{txt.to.words.ext}}, 
\code{\link{txt.to.features}}, \code{\link{make.samples}}
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
