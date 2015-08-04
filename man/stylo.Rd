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
stylo(gui = TRUE, frequencies = NULL, parsed.corpus = NULL,
      features = NULL, path = NULL, corpus.dir = "corpus", ...)
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{frequencies}{using this optional argument, one can load a custom
    table containing frequencies/counts for several variables, e.g. 
    most frequent words, across a number of text samples. It can be either 
    an R object (matrix or data frame), or a filename containing 
    tab-delimited data. If you use an R object, make sure that the rows
    contain samples, and the columns -- variables (words). If you use
    an external file, the variables should go vertically (i.e. in rows):
    this is because files containing vertically-oriented tables are far 
    more flexible and easily editable using, say, Excel or any text editor.
    To flip your table horizontally/vertically use the generic function t().}

  \item{parsed.corpus}{another option is to pass a pre-processed corpus
    as an argument. It is assumed that this object is a list, each element
    of which is a vector containing one tokenized sample. The example shown
    below will give you some hints how to prepare such a corpus.}

  \item{features}{usually, a number of the most frequent features (words,
    word n-grams, character n-grams) are extracted automatically from the
    corpus, and they are used as variables for further analysis. However,
    in some cases it makes sense to use a set of tailored features, e.g.
    the words that are associated with emotions or, say, a specific subset 
    of function words. This optional argument allows to pass either a
    filename containing your custom list of features, or a vector 
    (R object) of features to be assessed.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{corpus.dir}{the subdirectory (within the current working directory) that
    contains the corpus text files. If not specified, the default 
    subdirectory \code{corpus} will be used. This option is immaterial when
    an external corpus and/or external table with frequencies is loaded.}

  \item{...}{any variable produced by \code{stylo.default.settings}
    can be set here, in order to overwrite the default values. An example
    of such a variable is \code{network = TRUE} (switched off as default)
    for producing stylometric bootstrap consensus networks (Eder, forthcoming); 
    the function saves a csv file, containing a list of nodes that can be 
    loaded into, say, Gephi.}
}
\details{
If no additional argument is passed, then the functions tries to load
text files from the default subdirectory \code{corpus}. 
There are a lot of additional options that should be passed to this
function; they are all loaded when \code{stylo.default.settings} is 
executed (which is typically called automatically from inside the \code{stylo} 
function).
}
\value{
The function returns an object of the class \code{stylo.results}:
a list of variables, including a table of word frequencies, vector of features 
used, a distance table and some more stuff. Additionally, depending on which 
options have been chosen, the function produces a number of files containing 
results, plots, tables of distances, etc.
}
\references{
Eder, M. Kestemont, M. and Rybicki, J. (2013). Stylometry with R:
    a suite of tools. In: "Digital Humanities 2013: Conference Abstracts". 
    University of Nebraska-Lincoln, Lincoln, NE, pp. 487-89.
    
Eder, M. (forthcoming). Visualization in stylometry: some problems and 
    solutions.
}

\author{Maciej Eder, Jan Rybicki, Mike Kestemont}

\seealso{
\code{\link{classify}}, \code{\link{oppose}}, \code{\link{rolling.classify}}
}
\examples{
\dontrun{
# standard usage (it builds a corpus from a set of text files):
stylo()

# loading word frequencies from a tab-delimited file:
stylo(frequencies = "my_frequencies.txt")

# using an existing corpus (a list containing tokenized texts):
txt1 = c("to", "be", "or", "not", "to", "be")
txt2 = c("now", "i", "am", "alone", "o", "what", "a", "slave", "am", "i")
txt3 = c("though", "this", "be", "madness", "yet", "there", "is", "method")
custom.txt.collection = list(txt1, txt2, txt3)
  names(custom.txt.collection) = c("hamlet_A", "hamlet_B", "polonius_A")
stylo(parsed.corpus = custom.txt.collection)

# using a custom set of features (words, n-grams) to be analyzed:
my.selection.of.function.words = c("the", "and", "of", "in", "if", "into", 
                                   "within", "on", "upon", "since")
stylo(features = my.selection.of.function.words)

# loading a custom set of features (words, n-grams) from a file:
stylo(features = "wordlist.txt")

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
