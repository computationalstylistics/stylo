
# #################################################
# The generic function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). Alternatively, 
# you can replace it with another rule.
# Required argument: name of the text (string) to be split.
# ATTENTION: this is (almost) the only piece of coding in this script
# that dependens on the operating system used
# #################################################

txt.to.words <-
function(input.text) {
  # converting all the letters to lowercase
  input.text = tolower(input.text)
  # splitting into units specified by regular expression; here, 
  # all sequences between non-letter characters are assumed to be words:
  if(Sys.info()[["sysname"]] == "Windows") { 
    ### Windows
    tokenized.text = c(unlist(strsplit(input.text, "\\W+|_+",perl=T)))
  } else {
    ### Linux, Mac
    tokenized.text = c(unlist(strsplit(input.text, "[^[:alpha:]]+")))
  }
  return(tokenized.text)
}
