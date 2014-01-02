
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
function(input.text, splitting.rule = NULL) {
  # converting all the letters to lowercase
  input.text = tolower(input.text)
     # if no custom splitting rule was detected...
    if(length(splitting.rule) == 0 ) {
      # splitting into units specified by regular expression; here, 
      # all sequences between non-letter characters are assumed to be words:
      if(Sys.info()[["sysname"]] == "Windows") { 
        ### Windows
        tokenized.text = c(unlist(strsplit(input.text, "\\W+|_+",perl=T)))
      } else {
        ### Linux, Mac
        tokenized.text = c(unlist(strsplit(input.text, "[^[:alpha:]]+")))
      }
    # if custom splitting rule was indicated:
    } else {
      # sanity check
      if(length(splitting.rule) == 1) {
        # just in case, convert to characters
        splitting.rule = as.character(splitting.rule)
        # splitting into units specified by custom regular expression
        tokenized.text = c(unlist(strsplit(input.text, splitting.rule)))
      } else {
        stop("Wrong splitting regexp")
      }
    }
  # getting rid of emtpy strings
  tokenized.text = tokenized.text[nchar(tokenized.text) > 0]
# outputting the results
return(tokenized.text)
}
