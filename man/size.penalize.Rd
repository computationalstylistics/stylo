\encoding{utf8}
\name{size.penalize}
\alias{size.penalize}
\title{Testing Minimal Sample Size for Text Classification}
\description{
This function tests the ability of a given input text (or texts) to be 
correctly classified in a supervised machine-learning setup (e.g. Delta,
SVM or NSC) when its length is limited. The procedure, introduced by Eder 
(2017), involves several iterations in which longer and longer samples 
are drawn from the text in question, and then they are tested against 
a training set. For very short samples, the obtained classification accuracy 
is quite low (obviously), but then it usually increases until it finally 
reaches a point of saturation. The function \code{size.penalize} is aimed 
at indentifying such a saturation point.
}
\usage{
size.penalize(training.frequencies = NULL, test.frequencies = NULL,
              training.corpus = NULL, test.corpus = NULL,
              mfw = c(100, 200, 500), features = NULL, 
              path = NULL, corpus.dir = "corpus",
              sample.size.coverage = seq(100, 10000, 100),
              sample.with.replacement = FALSE,
              iterations = 100, classification.method = "delta",
              list.cutoff = 1000, ...)
}
\arguments{
  \item{training.frequencies}{using this optional argument, one can 
      load a custom table containing frequencies/counts for several variables, 
      e.g. most frequent words, across a number of text samples (for the 
      training set). It can be either 
      an R object (matrix or data frame), or a filename containing 
      tab-delimited data. If you use an R object, make sure that the rows
      contain samples, and the columns -- variables (words). If you use
      an external file, the variables should go vertically (i.e. in rows):
      this is because files containing vertically-oriented tables are far 
      more flexible and easily editable using, say, Excel or any text editor. 
      To flip your table horizontally/vertically use the generic function 
      \code{t()}.}

  \item{test.frequencies}{using this optional argument, one can 
      load a custom table containing frequencies/counts for the 
      test set. Further details: immediately above.}

  \item{training.corpus}{another option is to pass a pre-processed corpus
      as an argument (here: the training set). It is assumed that this object 
      is a list, each element of which is a vector containing one tokenized 
      sample. The example shown below will give you some hints how to prepare 
      such a corpus. Also, refer to \code{help(load.corpus.and.parse)}}

  \item{test.corpus}{if \code{training.corpus} is used, then you should also 
      prepare a similar R object containing the test set.}
  
  \item{mfw}{how many most frequent words (or other units) should be used
      as features to test the classifier? The default value is 
      \code{c(100,200,500)}, to assess three different ranges of MFWs.}

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
      an external corpus and/or external tables with frequencies are loaded.}

  \item{sample.size.coverage}{the procedure iteratively tests classification
      accuracy for different sample sizes. Feel free to modify the default
      value \code{c(100, 10000, 100)}, which tests samples for 100, 200, 300,
      ..., 10,000 words.}

  \item{sample.with.replacement}{if a tested sample size is bigger than the 
      text to be tested, then the procedure stops, obviously. To prevent
      such a situation, you might decide to draw your samples (n words)
      with replacement, which means that particular words can be picked 
      more than once (default value is \code{FALSE}).}

  \item{iterations}{each sample size of a given text is tested by extracting
      randomly n words from the text in N iterations (default being 100). Since 
      the procedure is random, a large(ish) number of iterations, say 100,
      allows for testing an actual behavior of a given sample size.}
  
  \item{classification.method}{the option invokes one of the classification
      methods provided by the package \code{stylo}. Choose one of the
      following: "delta", "svm", "knn", "nsc", "naivebayes".}
          
  \item{list.cutoff}{when texts are loaded from files, tokenized, and counted,
      it is all followed by building a table of frequencies. Since it is unlikely
      to analyze thousands of most frequent words (rather than 100 or, say, 500),
      it saves lots of time when the table of frequencies is trimmed. The default
      value is 1000 most frequent words.}

  \item{...}{any other argument, usually tokenization settings (via the 
      parameters \code{corpus.lang}, \code{features}, \code{ngram.size} etc.),
      or hyperparameters of different classification methods, such as 
      a distanse measure (for Delta), a cost function (for SVM), and so forth.}
}
\details{
If no additional argument is passed, then the function tries to load
text files from the default subdirectory \code{corpus}. The resulting 
object will then contain accuracy and diversity scores for all the texts.
}
\value{
The function returns an object of the class \code{stylo.results}:
a list of variables, including classification accuracy scores for each
tested text and each assessed sample size, Simpson's diversity index scores,
and the names of the texts analyzed. Use the generic function \code{summary}
to see the contents of the object. Use the generic function \code{plot}
to generate a tailored plot conveniently.
}
\references{
Eder, M. (2017). Short samples in authorship attribution: A new approach. 
    "Digital Humanities 2017: Conference Abstracts". Montreal: McGill 
    University, pp. 221–24, \url{https://dh2017.adho.org/abstracts/341/341.pdf}.

}

\author{Maciej Eder}

\seealso{
\code{\link{plot.sample.size}}, \code{\link{classify}}, \code{\link{imposters}}
}
\examples{
\dontrun{

# standard usage (it builds a corpus from a set of text files):
results = size.penalize()
plot(results)

}
}

