\name{parse.pos.tags}
\alias{parse.pos.tags}
\title{Extract POS-tags or Words from Annotated Corpora}
\description{Function for extracting textual data from annotated corpora.
It uderstands Stanford Tagger, TreeTagger TaKIPI (a tagger for Polish),
and Alpino (a tagger for Dutch) output formats. Either part-of-speech tags, 
or words, or lemmata can be extracted.}

\usage{
parse.pos.tags(input.text, tagger = "stanford", feature = "pos")
}

\arguments{
  \item{input.text}{any string of characters (e.g. vector) containing markup 
    tags that have to be deleted.}
  \item{tagger}{choose the input format: "stanford" for Stanford Tagger, 
    "treetagger" for TreeTagger, "takipi" for TaKIPI.}
  \item{feature}{choose "pos" (default), "word", or "lemma" (this one is not
    available for the Stanford-formatted input).}
}
\value{
  If the function is applied to a single text, then a vector of extracted
  features is returned. If it is applied to a corpus (a list, preferably
  of a class "stylo.corpus"), then a list of preprocessed texts are returned.
}
\author{
Maciej Eder
}
\seealso{
  \code{\link{load.corpus}}, \code{\link{txt.to.words}}, 
  \code{\link{txt.to.words.ext}}, \code{\link{txt.to.features}}
}
\examples{
text = "I_PRP have_VBP just_RB returned_VBN from_IN a_DT visit_NN 
  to_TO my_PRP$ landlord_NN -_: the_DT solitary_JJ neighbor_NN  that_IN 
  I_PRP shall_MD be_VB troubled_VBN with_IN ._. This_DT is_VBZ certainly_RB 
  a_DT beautiful_JJ country_NN !_. In_IN all_DT England_NNP ,_, I_PRP do_VBP 
  not_RB believe_VB that_IN I_PRP could_MD have_VB fixed_VBN on_IN a_DT 
  situation_NN so_RB completely_RB removed_VBN from_IN the_DT stir_VB of_IN 
  society_NN ._."

parse.pos.tags(text, tagger = "stanford", feature = "word")
parse.pos.tags(text, tagger = "stanford", feature = "pos")
  
}
%\keyword{text processing}
