
# #################################################
# Function for extracting relevant data structures
# from POS-tagged texts or corpora.
# type = "treetagger", "stanford", "takipi", "alpino" (pick one)
# #################################################

parse.pos.tags = function(input.text, 
                          tagger = "stanford", 
                          feature = "pos") {



    # starting an empty function: just in case
    extract.stuff = function(x) {NULL}


    if(tagger == "treetagger") {
        # the first column from a text (tab delimited) has to be extracted
        if(feature == "word") {
            #preprocessed.text = gsub("\t.*", "", input.text)
            extract.stuff = function(x) gsub("\t.*", "", x)
        }
        # the second column gets extracted
        if(feature == "pos") {
            extract.stuff = function(x) gsub(".*?\t([^\t]*).*", "\\1", x)
        }
        # the third column gets extracted
        # dropping 4th, 5th, ... (if they appear)
        if(feature == "lemma") {
            extract.stuff = function(x) gsub(".*\t.*\t([^\t]*).*", "\\1", x)
        }
    }    
    if(tagger == "stanford") {
        # extract the words, or elements preceding the _ element
        # (example: I_PRP have_VBP just_RB returned_VBN from_IN . . .)
        if(feature == "word") {
            extract.stuff = function(x) gsub("(_.*? )|(_.*?$)", " ", x)
        }
        # extract the tags only
        if(feature == "pos") {
            extract.stuff = function(x) gsub("( .*?_)|(^.*?_)", " ", x)
        }
    }
    if(tagger == "takipi") {
        # TaKIPI is a tagger for Polish, outputting its results in XML.
        #
        # extracting word forms
        if(feature == "word") {
            extract.stuff = function(x) {
                y = grep("<orth>(.*)</orth>", x, value = TRUE)
                z = gsub("<orth>(.*)</orth>", "\\1", y)
                return(z)
            }
        }
        # extracting lemmata
        if(feature == "lemma") {
            extract.stuff = function(x) {
                y = grep("<lex disamb=\"1\">", x, value = TRUE)
                z = gsub("<.*<base>(.+)</base>.*", "\\1", y)
                return(z)
            }
        }
        # extracting POS-tags
        if(feature == "pos") {
            extract.stuff = function(x) {
                y = grep("<lex disamb=\"1\">", x, value = TRUE)
                z = gsub("<lex disamb=\"1\"><base>.*</base><ctag>(.+)</ctag></lex>", "\\1", y)
                return(z)
            }
        }
    }
    if(tagger == "alpino") {
        # Alpino is a tagger for Dutch, outputting its results in XML.
        #
        # extracting word forms
        if(feature == "word") {
            extract.stuff = function(x) {
                y = grep("<s>.*lemma.*</s>", x, value = TRUE)
                z =unlist(strsplit(gsub("</?.+?>", "", y), "[[:punct:] ]+"))
                return(z)
            }
        }
        # extracting lemmata
        if(feature == "lemma") {
            extract.stuff = function(x) {
                y = grep("<s>.*lemma.*</s>", x, value = TRUE)
                z = gsub(".+? lemma=\"(\\w+)\".*?", "\\1 ", y)
                z = unlist(strsplit(gsub(" >.*", "", z), " "))
                return(z)
            }
        }
        # extracting POS-tags
        if(feature == "pos") {
            extract.stuff = function(x) {
                y = grep("<s>.*lemma.*</s>", x, value = TRUE)
                z = gsub(".+? ctag=\"(\\w+)\".*?", "\\1 ", y)
                z = unlist(strsplit(gsub("  .*", " ", z), " "))
                return(z)
            }
        }
    }

        

    # the core procedure follows

    # test if the dataset has a form of a single string (a vector)
    if(is.list(input.text) == FALSE) {
    # apply an appropriate replacement function
        preprocessed.text = extract.stuff(input.text)
    # if the dataset already is shaped as a list
    } else {
        # applying an appropriate function to a corpus:
        preprocessed.text = lapply(input.text, extract.stuff)
        class(preprocessed.text) = "stylo.corpus"
    }

return(preprocessed.text)
}

