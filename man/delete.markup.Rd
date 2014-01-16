\name{delete.markup}
\alias{delete.markup}
\title{Delete HTML or XML tags}
\description{Function for removing markup tags (e.g. HTML, XML) from a string 
  of characters. All XML markup is assumed to be compliant with the TEI guidelines (\url{http://www.tei-c.org/}).}

\usage{
delete.markup(input.text, markup.type = "plain")
}

\arguments{
  \item{input.text}{any string of characters (e.g. vector) containing markup 
    tags that have to be deleted.}
  \item{markup.type}{any of the following values: \code{plain} 
    (nothing will happen), \code{html} (all <tags> will be deleted as well 
    as HTML header), \code{xml} (TEI header, all strings between <note> </note> 
    tags, and all the tags will be deleted), \code{xml.drama} (as above;
    but, additionally, speaker's names will be deleted, or strings within each
    the <speaker> </speaker> tags), \code{xml.notitles} (as above; but, 
    additionally, all the chapter/section (sub)titles will be deleted, 
    or strings within each the <head> </head> tags).}
}
\details{
  This function needs to be used carefully: while a document formatted
  in compliance with the TEI guidelines will be parsed flawlessly, the cleaning up of an HTML
  page harvested randomly on the web might cause some side effects, e.g. the footers, 
  disclaimers, etc. will not be removed.
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
%\keyword{text processing}
