
# #################################################
# Function for combining single features (words
# or characters) into n-grams, or strings of n elements;
# e.g. character 2-grams of the sentence "This is a sentence"
# are as follows: "th", "hi", "is", "s ", " i", "is", etc.
# Required arguments: (1) name of the vector of words/chars,
# (2) size of the n-gram (default value is 1)
# #################################################

make.ngrams <-
function(input.text,ngram.size=1) {
  # sanity check
  if(ngram.size > length(input.text) ) {
    stop("something wrong with your sample: unable to make ",
          ngram.size,"-grams out of ", length(input.text), " element(s)") 
    }
  # variable initialization
  txt = c()
  # checking if any n-grams are wanted (i.e. if n-gram size > 1)
  if(ngram.size > 1) {
    txt = input.text
    for(n in 2:ngram.size) {
    # agglutinating subsequent words/chars into n-grams
    txt = stri_paste(txt[1:(length(txt)-1)],input.text[n:length(input.text)],sep = " ")
    }
  } else {
  # if n-gram size is set to 1, then nothing will happen
  txt = input.text
  }
return(txt)
}
