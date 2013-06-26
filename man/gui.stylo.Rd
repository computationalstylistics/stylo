\name{gui.stylo}
\alias{gui.stylo}
\title{GUI for stylo}
\description{
Graphical user interface for \code{stylo}. Actually, it does
not make any sense to use it as a standalone function, but no one can
prevent you, if you intend to do it. As a result, this function sets
several variables needed for \code{stylo}.
}
\usage{
gui.stylo()
}
\details{
The function invokes \code{stylo.default.settings} to initialize a number
of default variables. Then it reads the file \code{stylo_config.txt} (if the file
exists and can be found in the current directory) to overwrite the defaults.
Then a GUI box appears, allowing variables' customization. Refer to 
miniHOWTO (\url{https://sites.google.com/site/computationalstylistics/preprints/STYLO\%20R\%20Script\%20Mini\%20HOWTO.pdf?attredirects=0&d=1}) 
for a detailed explanation what the particular variables are for 
and how to use them.
}
\value{
When the function is done, it returns a list containing some 50 elements, 
or particular variables. 
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

my.variables = gui.stylo()
summary(my.variables)
}
}
\keyword{GUI}
