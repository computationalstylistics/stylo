\name{stylo.pronouns}
\alias{stylo.pronouns}
\title{List of pronouns}
\description{
This function returns a list of pronouns that can be used
as a stopword list for different linguistic/stylometric analyses.
}
\usage{
stylo.pronouns(language = "English")
}
\arguments{
  \item{language}{an optional argument specifying the language: available
    languages are \code{English}, \code{Latin}, \code{Polish}, \code{Dutch},
    \code{French}, \code{German}, \code{Spanish}, \code{Italian}, and
    \code{Hungarian} (default is \code{English}).}
}
\value{
The function returns a vector \code{pronouns}.
}
\references{
Hoover, D. (????) The paper where he discusses advantages of pronouns 
deletion. Literary and Linguistic Computing ....
}

\author{Jan Rybicki, Mike Kestemont}

\seealso{
\code{\link{stylo}}
}
\examples{
stylo.pronouns()
stylo.pronouns(language="Latin")
my.stop.words = stylo.pronouns(language="German")
}
\keyword{language processing}
