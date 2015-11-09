
# #################################################
# The generic function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). Alternatively, 
# you can replace it with another rule.
# Required argument: name of the text (string) to be split.
# ATTENTION: this is (almost) the only piece of coding in this script
# that dependens on the operating system used
# #################################################

txt.to.words = function(input.text, 
                        splitting.rule = NULL, 
                        preserve.case = FALSE) {
#


  # since the function can be applied to lists and vectors,
  # we need to define an internal function that will be applied afterwards
  wrapper = function(input.text = input.text, splitting.rule = splitting.rule, 
                        preserve.case = preserve.case) {

  # converting characters to lowercase if necessary
  if (!(preserve.case)){
      input.text = tryCatch(tolower(input.text), 
                            error=function(e) NULL)
      if(is.null(input.text) == TRUE) {
        input.text = "empty"
        cat("turning into lowercase failed!\n")
      }
  }
     # if no custom splitting rule was detected...
    if(length(splitting.rule) == 0 ) {
      # splitting into units specified by regular expression; here, 
      # all sequences between non-letter characters are assumed to be words:
      splitting.rule = paste("[^A-Za-z",
          # Latin supplement (Western):
          "\U00C0-\U00FF",
          # Latin supplement (Eastern):
          "\U0100-\U01BF",
          # Latin extended (phonetic):
          "\U01C4-\U02AF",
          # modern Greek:
          "\U0386\U0388-\U03FF",
          # Cyrillic:
          "\U0400-\U0481\U048A-\U0527",
          # Hebrew:
          "\U05D0-\U05EA\U05F0-\U05F4",
          # Arabic:
          "\U0620-\U065F\U066E-\U06D3\U06D5\U06DC",
          # extended Latin:
          "\U1E00-\U1EFF",
          # ancient Greek:
          "\U1F00-\U1FBC\U1FC2-\U1FCC\U1FD0-\U1FDB\U1FE0-\U1FEC\U1FF2-\U1FFC",
          # Coptic:
          "\U03E2-\U03EF\U2C80-\U2CF3",
          # Georgian:
          "\U10A0-\U10FF",
          "]+",
          sep="")
      tokenized.text = c(unlist(strsplit(input.text, splitting.rule)))
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
  
  }
 
  
  
        # the proper procedure applies, depending on what kind of data 
        # is analyzed
        
        # test if the dataset has a form of a single string (a vector)
        if(is.list(input.text) == FALSE) {
                # apply an appropriate replacement function
                tokenized.text = wrapper(input.text = input.text, 
                        splitting.rule = splitting.rule,
                        preserve.case = preserve.case)
                # if the dataset has already a form of list
        } else {
                # applying an appropriate function to a corpus:
                tokenized.text = lapply(input.text, wrapper, 
                        splitting.rule = splitting.rule,
                        preserve.case = preserve.case)
                class(tokenized.text) = "stylo.corpus"
        }
        
  
  
# outputting the results
return(tokenized.text)
}
