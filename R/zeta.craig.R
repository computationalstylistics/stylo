

# input.data -- two columns containing word counts in I and II set
# it should contain rownames
# filter.threshold

zeta.craig <-
function(input.data, filter.threshold) {
        
        comparison.primary = input.data[,1]
        comparison.secondary = input.data[,2]
        
        # two ways of expressing the same thing:
        differences = ( comparison.primary - comparison.secondary ) / 100 # note: add-one smoothing
        #differences = (comparison.primary + (100 - comparison.secondary )) / 100
        
        # extracting the distinctive words
        words.preferred = sort(differences[differences > 0 + filter.threshold],
                               decreasing=TRUE)
        words.avoided = sort(differences[differences < 0 - filter.threshold])
                
        # putting the results on a list
        results = list(words.preferred,words.avoided)
        names(results) = c("words_preferred", "words_avoided")
        
return(results)
}

