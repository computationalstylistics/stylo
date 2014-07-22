

# input.data -- two columns containing word counts in I and II set
# it should contain rownames
# filter.threshold

zeta.eder <-
function(input.data, filter.threshold) {
        
        comparison.primary = input.data[,1]
        comparison.secondary = input.data[,2]
        # computation of the differences between occurences of words
        differences = ( comparison.primary - comparison.secondary ) / 
                               ( comparison.primary + comparison.secondary )
        # getting rid of extreme values
        differences = differences[differences > -1]
        differences = differences[differences < 1]
        
        # extracting the distinctive words
        words.preferred = sort(differences[differences > 0 + filter.threshold],
                               decreasing = TRUE)
        words.avoided = sort(differences[differences < 0 - filter.threshold])

        # putting the results on a list
        results = list(words.preferred,words.avoided)
        names(results) = c("words_preferred", "words_avoided")
        
return(results)
}

