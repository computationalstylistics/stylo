\name{delete.markup}
\alias{delete.markup}
\title{Delete HTML or XML tags}
\description{Function for deleting markup (HTML, XML) from a string 
  of characters. XML markup is assumed to follow TEI guidelines.}

\usage{
delete.markup(input.text, markup.type = "plain")
}

\arguments{
  \item{input.text}{any string of characters (e.g. vector), presumably 
    containing some markup tags to be deleted}
  \item{markup.type}{choose one of the following values: \code{plain} 
    (nothing will happen), \code{html} (all tags will be deleted as well 
    as HTML header), \code{xml} (TEI header, any text between <note> </note> 
    tags, and all the tags will be deleted), \code{xml.drama} (as above;
    additionally speaker's names will be deleted, or strings within each
    pair of the tags <speaker> </speaker>).}
}
\details{
  This function needs to be used carefully: while a document formatted
  with an accordance to TEI guidelines will be parsed flawlessly, a HTML
  page harvested randomly on the web might cause some side effects, e.g.
  the footers, disclaimers, etc. will probably be not removed.
}
\author{
Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
  \code{\link{load.corpus}}, \code{\link{txt.to.words}}, 
  \code{\link{txt.to.words.ext}}, \code{\link{txt.to.features}}
}
\examples{
  delete.markup("Gallia est omnis <i>divisa</i> in partes tres", 
           markup.type = "html")

  delete.markup("Gallia<note>Gallia: Gaul.</note> est omnis 
           <emph>divisa</emph> in partes tres", markup.type = "xml")

  delete.markup("<speaker>Hamlet</speaker>Words, words, words...", 
           markup.type = "xml.drama")
}
\keyword{text processing}
