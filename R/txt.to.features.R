
# #################################################
# Function that carries out the necessary modifications
# for feature selection: convert an input text into
# the type of sequence needed (n-grams etc.) and
# returns the new list of items
# Argument: a vector of words (or chars)
# #################################################

txt.to.features <-
function(tokenized.text,features="w",ngram.size=1){
  #
  # Splitting the text into chars (if "features" was set to "c")
  if(features == "c") {
    sample = paste(tokenized.text, collapse=" ")
    sample = unlist(strsplit(sample,""))
# replacing  spaces with underscore
# it is a very proc time consuming task; thus, let's drop it
#    sample = gsub(" ","_",sample)
  } else {
  # otherwise, leaving the original text unchanged
  sample = tokenized.text
  }
  # 2. making n-grams (if an appropriate option has been chosen):
  if(ngram.size > 1) {
    sample = make.ngrams(sample,ngram.size)
#    # getting rid of additional spaces added around chars
#    # it is a very proc time consuming task; thus, let's drop it
#    if(features == "c") {
#      sample = gsub(" ","",sample)
#    }
  }
#
return(sample)
}
