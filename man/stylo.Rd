\name{stylo}
\alias{stylo}
\alias{stylo.package}
\title{Stylometric multidimensional analyses}
\description{
It is quite a long story what this function does. Basically, it is an 
all-in-one tool for a variety of experiments in computational
stylistics. For a more detailed description, refer to HOWTO available at:
\url{https://sites.google.com/site/computationalstylistics/}
}
\usage{
stylo(gui = TRUE, path = "", corpus.dir = "corpus", ...)
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{corpus.dir}{the subdirectory (within the current working directory) that
    contains the corpus files. If not specified, the default subdirectory \code{corpus} 
    will be used.}

  \item{...}{any variable produced by \code{stylo.default.settings}
    can be set here, in order to overwrite the default values.}
}
\details{
There are a lot of additional options that should be passed to this
function; they are all loaded when \code{stylo.default.settings} is 
executed (which is typically called automatically from inside the \code{stylo} 
function).
}
\value{
The function returns a list of variables, including a table of word frequencies, vector of features used, a distance table and some more stuff. Additionally, depending on which options have been chosen, the function produces a number of files containing results, plots, tables of distances, etc.
}
\references{
Eder, M. Kestemont, M. and Rybicki, J. (2013). Stylometry with R:
    a suite of tools. In: "Digital Humanities 2013: Conference Abstracts". 
    University of Nebraska-Lincoln, Lincoln, NE, pp. 487-89.
}

\author{Maciej Eder, Jan Rybicki, Mike Kestemont}

\seealso{
\code{\link{classify}}, \code{\link{oppose}}, \code{\link{rolling.delta}}
}
\examples{
\dontrun{
# standard usage:
stylo()

# batch mode, custom name of corpus directory:
my.test = stylo(gui = FALSE, corpus.dir = "ShakespeareCanon")
summary(my.test)

# batch mode, character 3-grams requested:
stylo(gui = FALSE, analyzed.features = "c", ngram.size = 3)

}
}
%\keyword{principal components analysis}
%\keyword{multidimensional scaling}
%\keyword{cluster analysis}
%\keyword{bootstrap consensus trees}
%\keyword{dendrogram}
%\keyword{scatterplot}
