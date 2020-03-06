
# #################################################
# The function for splitting a given input text into
# single words (chains of characters delimited with
# spaces or punctuation marks). Manages some 
# language-dependent text features during the process
# of splitting.
# Required argument: string of characters (words)
# Optional argument: corpus.lang
# Supported languages: "English", "English.contr",
# "English.all", "Latin.corr", ...
# #################################################

txt.to.words.ext <- function(input.text,
                             corpus.lang = "English",
                             splitting.rule = NULL,
                             preserve.case = FALSE) {
  
  # if no valid splitting rule, set splitting rule
  # based on corpus.lang
  if (length(splitting.rule) > 1) {
    stop("Wrong splitting regexp")
  } else if (length(splitting.rule) == 0) {
    splitting.rule = get.splitting.rule(corpus.lang)
  }
  
  # don't covert CJK corpora to lower case
  if (corpus.lang == 'CJK') {
    preserve.case <- TRUE
  }
  
  # process text: split, convert to lower case, a few
  # other lang-specific processing steps
  tokenized.text <- txt.processing(input.text,
                                   splitting.rule,
                                   preserve.case,
                                   corpus.lang)
  
  return(tokenized.text)
}

txt.processing <-
  function(input.text, splitting.rule, preserve.case, corpus.lang) {
    
    # converting characters to lowercase if necessary
    if (!(preserve.case)) {
      input.text = tryCatch(
        tolower(input.text),
        error = function(e)
          NULL
      )
      if (is.null(input.text) == TRUE) {
        input.text = "empty"
        cat("turning into lowercase failed!\n")
      }
    }
    
    # Special processing for English corpora:
    # getting rid of contractions ('t, 's, 've, 'd, 'll, 'em, 'im, 're) by
    # replacing their apostrophes with ^ (other apostrophes will not
    # be replaced); of course, if your corpus is Cockney, you should edit
    # the "([tsdm]|ll|ve|em|im)" statement accordingly.
    if (corpus.lang == 'english.contr' |
        corpus.lang == 'english.all') {
      input.text = gsub("([[:alpha:]])'([tsdm]|ll|ve|em|im|re)\\b",
                        "\\1^\\2",
                        input.text)
      # adding spaces around dashes (to distinguish dashes and hyphens)
      input.text = gsub("[-]{2,5}", " -- ", input.text)
    }
    
    # splitting the text
    input.text = c(unlist(stringr::str_split(input.text, splitting.rule)))
    
    # option for enlgish corpora:
    # trying to clean the remaining dashes
    if (corpus.lang == 'english.contr') {
      input.text = gsub("^[-]+$", "", input.text)
    }
    input.text = input.text[nchar(input.text) > 0]
    
    return(input.text)
  }

get.splitting.rule <- function(corpus.lang) {

  # maps corpus.lang to splitting.rule

  if (corpus.lang == 'CJK') {
    splitting.rule = paste(
      "[^A-Za-z",
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
      "]+",
      sep = ""
    )
  } else if (corpus.lang == 'english.contr') {
    splitting.rule = "[^A-Za-z\U00C0-\U00FF\U0100-\U01BF\U01C4-\U02AF^]+"
  } else if (corpus.lang == 'english.all') {
    splitting.rule = "[^A-Za-z\U00C0-\U00FF\U0100-\U01BF\U01C4-\U02AF^-]+"
  } else {
    splitting.rule = paste(
      "[^A-Za-z",
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
      # Arabic/Farsi:
      "\U0620-\U065F\U066E-\U06D3\U06D5\U06DC",
      # extended Latin:
      "\U1E00-\U1EFF",
      # ancient Greek:
      "\U1F00-\U1FBC\U1FC2-\U1FCC\U1FD0-\U1FDB\U1FE0-\U1FEC\U1FF2-\U1FFC",
      # Coptic:
      "\U03E2-\U03EF\U2C80-\U2CF3",
      # Georgian:
      "\U10A0-\U10FF",
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
      "]+",
      sep = ""
    )
  }
  return(splitting.rule)
}