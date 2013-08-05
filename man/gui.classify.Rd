\name{gui.classify}
\alias{gui.classify}
\title{GUI for classify}
\description{
Graphical user interface for \code{classify}. Via the GUI, this function can set
the several variables needed for \code{classify}.
}
\usage{
gui.classify()
}
\details{
The function calls \code{stylo.default.settings} to initialize a number
of default variables. Then it reads the file \code{classify_config.txt} 
(if the file exists and can be found in the current directory) to 
overwrite any default values. Then a GUI box appears, allowing the variables' 
customization by the user. Refer to miniHOWTO (\url{https://sites.google.com/site/computationalstylistics/preprints/STYLO\%20R\%20Script\%20Mini\%20HOWTO.pdf?attredirects=0&d=1}) 
for a detailed explanation what the particular variables are for 
and how to use them.
}
\value{
The function returns a list containing some 50 variables. 
}
\author{
Jan Rybicki
}
\seealso{
\code{\link{classify}}, \code{\link{gui.stylo}}
}
\examples{
\dontrun{
gui.classify()

my.variables = gui.classify()
summary(my.variables)
}
}
%\keyword{GUI}
