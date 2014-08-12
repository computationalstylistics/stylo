
# #################################################
# Function for adjusting different input formats:
# xml (TEI) in two variants, html, and plain text files.
# Required arguments: (1) name of the text to pre-process,
# (2) type of markup to delete (plan txt is default)
# #################################################

delete.markup <-
function(input.text, markup.type = "plain") {
  if(markup.type == "xml" || markup.type == "xml.drama" 
                          || markup.type == "xml.notitles") {
    # getting rid of the TEI header (if it exists)
    if(length(grep("</tei[Hh]eader>", input.text)) > 0) {
      input.text = input.text[-c(1:(grep("</tei[Hh]eader>",input.text)))]
      }
    # the whole text into one (very) long line
    preprocessed.text = paste(input.text, collapse=" ")
    # getting rid of dramatis personae
    if(markup.type == "xml.drama" || markup.type == "xml.notitles"){
      preprocessed.text = gsub("<speaker>.*?</speaker>","",preprocessed.text)
      }
    # getting rid of comments and (editorial) notes
    preprocessed.text = gsub("<note.*?</note>","",preprocessed.text)
    # additionally (for xml.notitles), getting rid of titles
    if(markup.type == "xml.notitles") {
      preprocessed.text = gsub("<head.*?</head>","",preprocessed.text)
    }
    # getting rid of all the remaining tags
    preprocessed.text = gsub("<.*?>","",preprocessed.text)
  } else if(markup.type == "html") {
    # getting rid of html header (if exists)
    if(length(grep("<body",input.text)) > 0) {
      input.text = input.text[-c(1:(grep("<body",input.text)))]
      }
    # the whole text into one (very) long line
    preprocessed.text = paste(input.text, collapse=" ")
    # getting rid of links (menus and similar stuff should be deleted, hopefully)
    preprocessed.text = gsub("<a href.*?/a>","",preprocessed.text)
    # getting rid of all the remaining tags
    preprocessed.text = gsub("<.*?>","",preprocessed.text)
  } else {
  preprocessed.text = input.text
  }
return(preprocessed.text)
}
