
# #################################################
# The function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). It is build on top
# of the function txt.to.words() and it is designed
# to manage some language-dependent text features 
# during the process of splitting.
# Required argument: string of characters (words)
# Optional argument: corpus.lang
# Supported languages: "English", "English.contr",
# "English.all", "Latin.corr", ...
# #################################################

txt.to.words.ext = function(input.text, 
         corpus.lang = "English", 
         splitting.rule = NULL,
         preserve.case = FALSE) {

                 
     # since the function can be applied to lists and vectors,
     # we need to define an internal function that will be applied afterwards
     wrapper = function(input.text = input.text, 
                        corpus.lang = corpus.lang, 
                        splitting.rule = splitting.rule,
                        preserve.case = preserve.case) {
                 
                 
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
    if(tolower(corpus.lang) == "cjk") {
      tokenized.text = txt.to.words(input.text,  
                                    splitting.rule = paste("[^A-Za-z",
                                        # Japanese (Hiragana)
                                        "\U3040-\U309F",
                                        # Japanese (Katagana):
                                        "\U30A0-\U30FF",
                                        # Japanese repetition symbols:
                                        "\U3005\U3031-\U3035",
                                        # CJK Unified Ideographs: 
                                        "\U4E00-\U9FFF",
                                        # CJK Unified Ideographs Extension A: 
                                        "\U3400-\U4DBF",
                                        # Hangul (Korean script):
                                        "\UAC00-\UD7AF",
                                        "]+", sep=""),
                                    preserve.case=TRUE)
      }
    if(tolower(corpus.lang) != "english.contr" & 
       tolower(corpus.lang) != "english.all" & tolower(corpus.lang) != "cjk" ) {
    tokenized.text = txt.to.words(input.text, preserve.case=preserve.case)
    }
    # if the Latin option with adjusting the v/u letters is on,
    # this smashes the distinction and converts both types to the letter u
    if(tolower(corpus.lang) == "latin.corr") {
      tokenized.text = gsub("v", "u", tokenized.text)
      tokenized.text = gsub("&", "et", tokenized.text)
      tokenized.text = gsub("j", "i", tokenized.text)
    }
    # this code is used for English corpora only
    if(tolower(corpus.lang) == "english.contr" | tolower(corpus.lang) == "english.all") {
      # turning into lowercase, if applicable
        if(preserve.case == FALSE) {
          input.text = tolower(input.text)
        }
      # replacing non-ASCII apostrophes with simple ' (standard ASCII char)
### TEMPORARILY SWITCHED OFF, due to some CRAN restrictions
### tokenized.text = gsub(iconv("\u2019",from="UTF-8"),"'",input.text)
tokenized.text = input.text
      # getting rid of contractions ('t, 's, 've, 'd, 'll, 'em, 'im, 're) by 
      # replacing their apostrophes with ^ (other apostrophes will not 
      # be replaced); of course, if your corpus is Cockney, you should edit 
      # the "([tsdm]|ll|ve|em|im)" statement accordingly.
      tokenized.text = gsub("([[:alpha:]])'([tsdm]|ll|ve|em|im|re)\\b","\\1^\\2",
                            tokenized.text) 
      # adding spaces around dashes (to distinguish dashes and hyphens)
      tokenized.text = gsub("[-]{2,5}"," -- ",tokenized.text)
      # depending on which option was swithed on, either the contractions are
      # kept, or all the peculiarities, i.e. both contractions and hyphens
        # character class covering all scripts supported by txt.to.words.R
        unicode.letters = paste("[^A-Za-z",
            "\U00C0-\U00FF",
            "\U0100-\U01BF",
            "\U01C4-\U02AF",
            "\U0386\U0388-\U03FF",
            "\U0400-\U0481\U048A-\U0527",
            "\U05D0-\U05EA\U05F0-\U05F4",
            "\U0620-\U065F\U066E-\U06D3\U06D5\U06DC",
            "\U1E00-\U1EFF",
            "\U1F00-\U1FBC\U1FC2-\U1FCC\U1FD0-\U1FDB\U1FE0-\U1FEC\U1FF2-\U1FFC",
            "\U03E2-\U03EF\U2C80-\U2CF3",
            "\U10A0-\U10FF",
            "\U3040-\U309F",
            "\U30A0-\U30FF",
            "\U3005\U3031-\U3035",
            "\U4E00-\U9FFF",
            "\U3400-\U4DBF",
            "\UAC00-\UD7AF",
            sep="")
        if(tolower(corpus.lang) == "english.contr") {
          tokenized.text = c(unlist(strsplit(tokenized.text,
                    paste0(unicode.letters, "^]+"))))
        }
        if(tolower(corpus.lang) == "english.all") {
          tokenized.text = c(unlist(strsplit(tokenized.text,
                    paste0(unicode.letters, "^-]+"))))
          # trying to clean the remaining dashes:
          tokenized.text = gsub("^[-]+$", "", tokenized.text)
        }
    }
    # trying to avoid empty strings:
    tokenized.text = tokenized.text[nchar(tokenized.text)>0]
    #
  }
  
  }
  

        # the proper procedure applies, depending on what kind of data 
        # is analyzed
        
        # test if the dataset has a form of a single string (a vector)
        if(is.list(input.text) == FALSE) {
                # apply an appropriate replacement function
                tokenized.text = wrapper(input.text = input.text, 
                        corpus.lang = corpus.lang, 
                        splitting.rule = splitting.rule,
                        preserve.case = preserve.case)
                # if the dataset has already a form of list
        } else {
                # applying an appropriate function to a corpus:
                tokenized.text = lapply(input.text, wrapper, 
                        corpus.lang = corpus.lang, 
                        splitting.rule = splitting.rule,
                        preserve.case = preserve.case)
                class(tokenized.text) = "stylo.corpus"
        }
        

  
return(tokenized.text)
}
