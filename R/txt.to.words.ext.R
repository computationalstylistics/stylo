
# #################################################
# The function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). It is build on top
# of the function txt.to.words() and it is designed
# to manage some language-dependent text features 
# during the process of splitting.
# Required argument: string of characters (words)
# Optional argument: language
# Supported languages: "English", "English.contr",
# "English.all", "Latin.corr", ...
# #################################################

txt.to.words.ext <-
function(input.text, 
         language="English", 
         splitting.rule = NULL,
         preserve.case = FALSE) {
  # if a custom splitting rule was detected...
  if(length(splitting.rule) > 0) {
      # sanity check
      if(length(splitting.rule) == 1) {
        # just in case, convert to characters
        splitting.rule = as.character(splitting.rule)
        # splitting into units specified by custom regular expression
        tokenized.text = txt.to.words(input.text, splitting.rule, preserve.case)
      } else {
        stop("Wrong splitting regexp")
      }
  
  # if no custom splitting rule was detected...
  } else {
    # Loading the file; optionally, fiddling with apostrophes and contractions:    #
    # This is the standard procedure of splitting input texts
    if(language != "English.contr" & language != "English.all") {
      tokenized.text = txt.to.words(input.text, preserve.case=preserve.case)
    }
    # if the Latin option with adjusting the v/u letters is on,
    # this smashes the distinction and converts both types to the letter u
    if(language == "Latin.corr") {
      tokenized.text = gsub("v","u",tokenized.text)
    }
    # this code is used for English corpora only
    if(language == "English.contr" | language == "English.all") {
      # turning into lowercase, if applicable
        if(preserve.case == FALSE) {
          input.text = tolower(input.text)
        }
      # replacing non-ASCII apostrophes with simple ' (standard ASCII char)
      tokenized.text = gsub(iconv("\u2019",from="UTF-8"),"'",input.text)
      # getting rid of contractions ('t, 's, 've, 'd, 'll, 'em, 'im) by 
      # replacing their apostrophes with ^ (other apostrophes will not 
      # be replaced); of course, if your corpus is Cockney, you should edit 
      # the "([tsdm]|ll|ve|em|im)" statement accordingly.
      tokenized.text = gsub("([[:alpha:]])'([tsdm]|ll|ve|em|im)\\b","\\1^\\2",
                            tokenized.text) 
      # adding spaces around dashes (to distinguish dashes and hyphens)
      tokenized.text = gsub("[-]{2,5}"," -- ",tokenized.text)
      # depending on which option was swithed on, either the contractions are
      # kept, or all the peculiarities, i.e. both contractions and hyphens
        if(language == "English.contr") {
          tokenized.text=c(unlist(strsplit(tokenized.text,"[^[:alpha:]^]+")))
        }
        if(language == "English.all") {
          tokenized.text=c(unlist(strsplit(tokenized.text,"[^[:alpha:]^-]+")))
          # trying to clean the remaining dashes:
          tokenized.text = gsub("^[-]+$","",tokenized.text)
        }
    }
    # trying to avoid empty strings:
    tokenized.text = tokenized.text[nchar(tokenized.text)>0]
    # trying to get rid of non-letter characters:
    tokenized.text = tokenized.text[grep("[^[:digit:]]",tokenized.text)]
    #
  }
return(tokenized.text)
}
