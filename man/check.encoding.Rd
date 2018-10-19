\encoding{utf8}
\name{check.encoding}
\alias{check.encoding}
\title{Check character encoding in corpus folder}
\description{
The default character encoding in stylo is UTF-8, deviating from it can 
cause problems. This function allows users to check the character 
encoding in a corpus. A summary if returned to the termial and a detailed
list reporting the most probable encodings of all the text files in the
folder are written to a csv file. The function is basically a wrapper 
around the function \code{guess_encoding()} from the 'readr' package by
Wickham et al. (2017). To change the encoding to UTF-8, try the 
\code{change.encoding()} function.
}
\usage{
check.encoding(corpus.dir = "corpus/", output.file = "encodings.csv")
}
\arguments{
  \item{corpus.dir}{path to the folder containing the corpus.}

  \item{output.file}{path to a csv file that reprots the most probable encoding
  for each text file in the corpus.}
}
\details{
If no additional argument is passed, then the function tries to check the
text files in the default subdirectory \code{corpus}. 
}
\value{
The function returns a summary message and writes detailed results into a csv file.
}
\references{
Wickham , H., Hester, J., Francois, R., Jylanki, J., and Jørgensen, M. (2017). Package: 'readr'. https://cran.r-project.org/web/packages/readr/readr.pdf.
}

\author{Steffen Pielström}

\seealso{
\code{\link{change.encoding}}
}
\examples{
\dontrun{
# standard usage from stylo working directory with a \code {corpus/} subfolder:
check.encoding()

# specifying another folder:
check.encoding("~/corpora/example1/")

# specifying both input folder and output file:
check.encoding("~/corpora/example1/", output.file = "~/experiments/charencoding/example1.csv")

}
}
%\keyword{character encoding}