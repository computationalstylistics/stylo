\name{stylo.pronouns}
\alias{stylo.pronouns}
\title{List of pronouns}
\description{
This function returns a list of pronouns that can be used
as a stop word list for different stylometric analyses.
It has been shown that pronoun deletion improves, to some extent,
attribution accuracy of stylometric procedures (e.g. in English novels: Hoover 2004a; 2004b).
}
\usage{
stylo.pronouns(corpus.lang = "English")
}
\arguments{
  \item{corpus.lang}{an optional argument specifying the language of the texts 
      analyzed: available languages are \code{English}, \code{Latin}, 
      \code{Polish}, \code{Dutch}, \code{French}, \code{German}, \code{Spanish}, 
      \code{Italian}, and \code{Hungarian} (default is \code{English}).}
}
\value{
The function returns a vector of pronouns.
}
\references{
Hoover, D. (2004a). Testing Burrows's delta. "Literary and Linguistic 
    Computing", 19(4): 453-75.

Hoover, D. (2004b). Delta prime?. "Literary and Linguistic Computing", 
    19(4): 477-95.
}

\author{Jan Rybicki, Maciej Eder, Mike Kestemont}

\seealso{
\code{\link{stylo}}
}
\examples{
stylo.pronouns()
stylo.pronouns(corpus.lang = "Latin")
my.stop.words = stylo.pronouns(corpus.lang = "German")
}
%\keyword{text processing}
%\keyword{corpus linguistics}
