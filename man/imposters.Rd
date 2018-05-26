\name{imposters}
\alias{imposters}
\title{Authorship Verification Classifier Known as the Imposters Method}
\description{
A machine-learning supervised classifier tailored to assess authorship
verification tasks. This function is an implementation of the 2nd order
verification system known as the General Imposters framework (GI), 
and introduced by Koppel and Winter (2014). The current implementation 
tries to stick -- with some improvements -- to the description provided 
by Kestemont et al. (2016: 88).
}
\usage{
imposters(reference.set, 
          test = NULL,
          candidate.set = NULL,
          iterations = 100,
          features = 0.5,
          imposters = 0.5,
          classes.reference.set = NULL,
          classes.candidate.set = NULL,
          ...) 
}
\arguments{
\item{reference.set}{a table containing frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of texts
          written by different authors. It is really important to put there
          a selection of "imposters", or the authors that could not have written
          the text to be assessed. If no \code{candidate.set} is used, then the
          table should also contain some texts written by possible candidates to 
          authorship, or the authors that are suspected of being the actual author. 
          Make sure that the rows contain samples, and the columns -- 
          variables (words, n-grams, or whatever needs to be analyzed).}

\item{test}{a text to be checked for authorship, represented as a vector
          of, say, word frequencies. The variables used (i.e. columns) 
          must match the columns of the reference set. If nothing is indicated, 
          then the function will try to infer the test text from the 
          \code{reference.set}; when worse comes to worst, the first text 
          in the reference set will be excluded as the test text.}

\item{candidate.set}{a table containing frequencies/counts for the candidate set.
	      This set should contain texts written by possible candidates to 
          authorship, or the authors that are suspected of being the actual author.
          The variables used (i.e. columns) must match the columns of the 
          reference set. If no \code{candidate.set} is indicated, the function
          will test iteratively all the classes (one at a time) from the reference
          set.}

\item{iterations}{the model is rafined in N iterations. A reasonable number 
          of turns is a few dozen or so (see the argument "features" below).}

\item{features}{a proportion of features to be analyzed. The imposters method 
          selects randomly, in N iterations, a given subset of features 
          (words, n-grams, etc.) and performs a classification. It is assumed that
          a large number of iteration, each involving a randomly selected subset
          of features, leads to a reliable coverage of features, among which
          some outliers might be hidden. The argument specifies the
          proportion of features to be randomly chosen; the indicated value 
          should lay in the range between 0 and 1 (the default being 0.5).}

\item{imposters}{a proportion of text by the imposters to be analyzed. In each 
          iteration, a specified number of texts from the comparison set is chosen 
          (randomly). See above, for the features' choice. The default value
          of this parameter is 0.5.}

\item{classes.reference.set}{a vector containing class identifiers for the
          reference set. When missing, the row names of the set
          table will be used; the assumed classes are the strings of 
          characters followed by the first underscore. Consider the following 
          examples: c("Sterne_Tristram",
          "Sterne_Sentimental", "Fielding_Tom", ...), where the classes
          are the authors' names, and c("M_Joyce_Dubliners",
          "F_Woolf_Night_and_day", "M_Conrad_Lord_Jim", ...), where the
          classes are M(ale) and F(emale) according to authors' gender.
          Note that only the part up to the first underscore in the
          sample's name will be included in the class label.}

\item{classes.candidate.set}{a vector containing class identifiers for the
          candidate set. When missing, the row names of the set
          table will be used (see above).}

\item{...}{any other argument that can be passed to the classifier; see 
          \code{perform.delta} for the parameters to be tweaked. In the current
          version of the function, only distance measure used for computing 
          similarities between texts can be set. Available options so far: "delta" 
          (Burrows's Delta, default), "argamon" (Argamon's Linear Delta), 
          "eder" (Eder's Delta), "simple" (Eder's Simple Distance), 
          "canberra" (Canberra Distance), "manhattan" (Manhattan 
          Distance), "euclidean" (Euclidean Distance), "cosine"
          (Cosine Distance), "wurzburg" (Cosine Delta), "minmax" 
          (Minmax Distance, also known as the Ruzicka measure).
          }
}
\value{
The function returns a single score indicating the probability that an
anonymouns sample analyzed was/wasn't written by a candidate author. 
As a proportion, the score lies between 0 and 1 (higher scores indicate a higher 
attribution confidence). If more than one class is assessed, the resulting scores
are returned as a vector.
}
\references{
Koppel, M. , and Winter, Y. (2014). Determining if two documents are 
    written by the same author. "Journal of the Association for Information
    Science and Technology", 65(1): 178-187.

Kestemont, M., Stover, J., Koppel, M., Karsdorp, F. and Daelemans, W. (2016).
    Authenticating the writings of Julius Caesar. "Expert Systems With
    Applications", 63: 86-96.
}
\author{
Maciej Eder
}
\seealso{
\code{\link{perform.delta}}, \code{\link{imposters.optimize}}
}
\examples{
\dontrun{
# performing the imposters method on the dataset provided by the package:

# activating the datasets with "The Cuckoo's Calling", possibly written by JK Rowling
data(galbraith)

# running the imposters method against all the remaining authorial classes
imposters(galbraith)

# general usage:

# Let's assume there is a table with frequencies, the 8th row of which contains
# the data for a text one wants to verify.

# getting the 8th row from the dataset
text_to_be_tested = dataset[8,]

# building the reference set so that it does not contain the 8th row
remaining_frequencies = dataset[-c(8),]

# launching the imposters method:
imposters(reference.set = remaining_frequencies, test = text_to_be_tested)
}
}
%\keyword{classification}
