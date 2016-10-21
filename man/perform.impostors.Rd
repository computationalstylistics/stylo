\name{perform.impostors}
\alias{perform.impostors}
\title{An Authorship Verification Classifier Known as the Impostors Method}
\description{
A machine-learning supervised classifier tailored to assess authorship
verification tasks. This function is an implementation of the 2nd order
verification system known as the General Impostors framework (GI), 
and introduced by Koppel and Winter (2014). The current implementation 
tries to stick -- as closely as possible -- to the description provided 
by Kestemont et al. (2016: 88).
}
\usage{
perform.impostors(candidate.set, impostors.set, iterations = 100,
               features = 50, impostors = 30,
               classes.candidate.set = NULL, classes.impostors.set = NULL,
               distance = "delta", z.scores.both.sets = TRUE) 
}
\arguments{
\item{candidate.set}{a table containing	frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of texts
          written by a target author (i.e. the candidate to authorship). 
          This table should also contain an anonymous sample to be assessed.
          Make sure that the rows contain samples, and the columns -- 
          variables (words, n-grams, or whatever needs to be analyzed).}

\item{impostors.set}{a table containing frequencies/counts for the control set.
	      This set should contain the samples by the impostors, or the 
	      authors that could not have written the anonymous sample in question.
          The variables used (i.e. columns) must match the columns of the 
          candidate set.}

\item{iterations}{the model is rafined in N iterations. A reasonable number 
          of turns is a few dozen or so (see the argument "features" below).}

\item{features}{the "impostors" method is sometimes referred to as a 2nd order
          authorship verification system, since it selects randomly, in N
          iterations, a given subset of features (words, n-grams, etc.)
          and performs a classification. This argument specifies the
          percentage of features to be randomly chosen; the default value
          is 50.}

\item{impostors}{in each iteration, a specified number of texts from the 
          control set is chosen (randomly). The default number is 30.}

\item{classes.candidate.set}{a vector containing class identifiers for the
          authorial set. When missing, the row names of the set
          table will be used; the assumed classes are the strings of 
          characters followed by the first underscore. Consider the following 
          examples: c("Sterne_Tristram",
          "Sterne_Sentimental", "Fielding_Tom", ...), where the classes
          are the authors' names, and c("M_Joyce_Dubliners",
          "F_Woolf_Night_and_day", "M_Conrad_Lord_Jim", ...), where the
          classes are M(ale) and F(emale) according to authors' gender.
          Note that only the part up to the first underscore in the
          sample's name will be included in the class label.}

\item{classes.impostors.set}{a vector containing class identifiers for the
          control set. When missing, the row names of the set
          table will be used (see above).}

\item{distance}{a kernel (i.e. a distance measure) used for computing 
          similarities between texts. Available options so far: "delta" 
          (Burrow's Delta, default), "argamon" (Argamon's Linear Delta), 
          "eder" (Eder's Delta), "simple" (Eder's Simple Distance), 
          "canberra" (Canberra Distance), "manhattan" (Manhattan 
          Distance), "euclidean" (Euclidean Distance), "cosine"
          (Cosine Distance). THIS OPTION WILL BE CHANGED IN NEXT VERSIONS.}

\item{z.scores.both.sets}{many distance measures convert input variables into 
          z-scores before computing any distances. Such a variable
          weighting is highly dependent on the number of input texts. One might
          choose either training set only to scale the variables, or the entire
          corpus (both sets). The latter is default.
          THIS OPTION WILL BE CHANGED (OR DELETED) IN NEXT VERSIONS.}
}
\value{
The function returns a single score indicating the probability that an
anonymouns sample analyzed was/wasn't written by a candidate author. As a proportion, the score lies between 0 and 1 (higher scores indicate a higher attribution confidence).
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
\code{\link{perform.delta}}
}
%\keyword{classification}
