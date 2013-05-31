\name{gui.stylo}
\alias{gui.stylo}
\title{GUI for stylo}
\description{
Graphical user interface for \code{stylo}. Actually, it does
not make any sense to use it as a standalone function, but noone can
prevent you, if you intend to do it. A number of variables will be set.
}
\usage{
gui.stylo()
}
\details{
The function invokes \code{stylo.default.settings} to inicialize a number
of default variables. Then it reads the file \code{stylo_config.txt} (if the file
exists and can be found in the current directory) to overwrite the defaults.
Then a GUI box appears, allowing variables' customization. Refer to 
miniHOWTO (what about a http link?) for a detailed explanation what 
the particular variables are for and how to use them.
}
\value{
When the function is done, several global variables are set and ready 
to use. Type ls() to see them all.
}
\author{
Jan Rybicki
}
\seealso{
\code{\link{stylo}}, \code{\link{stylo.default.settings}}
}
\examples{
\dontrun{
gui.stylo()
ls()
}
}
\keyword{variables}
\keyword{gui}
