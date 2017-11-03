\name{oppose}
\alias{oppose}
\title{Contrastive analysis of texts}
\description{
Function that performs a contrastive analysis between two given sets 
of texts. It generates a list of words significantly preferred by 
a tested author (or, a collection of authors), and another list containing 
the words significantly avoided by the former when compared to another set 
of texts. Some visualizations are available.
}
\usage{
oppose(gui = TRUE, path = NULL, 
         primary.corpus = NULL,
         secondary.corpus = NULL,
         test.corpus = NULL,
         primary.corpus.dir = "primary_set",
         secondary.corpus.dir = "secondary_set",
         test.corpus.dir = "test_set", ...)
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current working directory will be used 
    for input/output procedures (reading files, outputting the results, etc.).}
    
  \item{primary.corpus.dir}{the subdirectory (within the current working 
    directory) that contains one or more texts to be compared to a comparison 
    corpus. These texts can e.g. be the oeuvre by author A (to be compared 
    to the oeuvre of another author B) or a collection of texts by female 
    authors (to be contrasted with texts by male authors). If not specified, 
    the default subdirectory \code{primary_set} will be used.}

  \item{secondary.corpus.dir}{the subdirectory (within the current working 
    directory) that contains a comparison corpus: a pool of texts to be 
    contrasted with texts from the \code{primary.corpus}. If not specified, 
    the default subdirectory \code{secondary_set} will be used.}

  \item{test.corpus.dir}{the subdirectory (within the current working directory)
    that contains texts to verify the discriminatory strength of the features
    extracted from the \code{primary.set} and \code{secondary.sets}. Ideally, 
    the \code{test.corpus.dir} should contain texts known to belong to both 
    classes (e.g. texts written by female and male authors in the case of 
    a gender-oriented study). If not specified, the default subdirectory 
    \code{test_set} will be used. If the default subdirectory does not exist 
    or does not contain any texts, the validation test will not be performed.}

  \item{primary.corpus}{another option is to pass a pre-processed corpus
    as an argument (here: the primary set). It is assumed that this object 
    is a list, each element of which is a vector containing one tokenized 
    sample. Refer to \code{help(load.corpus.and.parse)} to get some hints 
    how to prepare such a corpus.}

  \item{secondary.corpus}{if \code{primary.corpus} is used, then you should also 
    prepare a similar R object containing the secondary set.}
    
  \item{test.corpus}{if you decide to use test corpus, you can pass it as 
    a pre-processed R object using this argument.}
    
  \item{...}{any variable produced by \code{stylo.default.settings} can be set
    here, in order to overwrite the default values.}
}
\details{
This function performs a contrastive analysis between two given sets of texts, 
using Burrows's Zeta (2007) in its different flavors, including Craig's 
extensions (Craig and Kinney, 2009). Also, the Whitney-Wilcoxon procedure
as introduced by Kilgariff (2001) is available. The function generates 
a vector of words significantly preferred by a tested author, and another 
vector containing the words significantly avoided.
}
\value{
The function returns an object of the class \code{stylo.results}:
a list of variables, including a list of words significantly preferred in the 
primary set, words significantly avoided (or, preferred in the secondary set), 
and possibly some other results, if applicable.
}
\references{
Eder, M., Rybicki, J. and Kestemont, M. (2016). Stylometry with R: a package 
    for computational text analysis. "R Journal", 8(1): 107-21.

Burrows, J. F. (2007). All the way through: testing for authorship 
    in different frequency strata. "Literary and Linguistic Computing", 
    22(1): 27-48.

Craig, H. and Kinney, A. F., eds. (2009). Shakespeare, Computers, and the 
    Mystery of Authorship. Cambridge: Cambridge University Press.

Hoover, D. (2010). Teasing out authorship and style with t-tests and
    Zeta. In: "Digital Humanities 2010: Conference Abstracts". 
    King's College London, pp. 168-170.

Kilgariff A. (2001). Comparing Corpora. "International Journal of Corpus
    Linguistics" 6(1): 1-37.
}

\author{Maciej Eder, Mike Kestemont}

\seealso{
\code{\link{stylo}}, \code{\link{classify}}, \code{\link{rolling.classify}}
}
\examples{
\dontrun{
# standard usage:
oppose()

# batch mode, custom name of corpus directories:
oppose(gui = FALSE, primary.corpus.dir = "ShakespeareCanon",
       secondary.corpus.dir = "MarloweSamples")
}
}
%\keyword{Zeta}
%\keyword{corpus linguistics}
