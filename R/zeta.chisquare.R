


zeta.chisquare = function(input.data) {
        
        differences = input.data[,1] - input.data[,2]
        positive.values = differences[differences > 0]
        negative.values = differences[differences < 0]
        
        # selecting positive differences (=words preferred by primary author)
        positive.differences = input.data[names(positive.values),]
        # selecting negative differences (=words avoided by primary author)
        negative.differences = input.data[names(negative.values),]
        
        # preferred words
        b = NULL
        for(i in 1: length(positive.differences[,1])) {
                a = chisq.test(c(positive.differences[i,1],positive.differences[i,2]))$p.value
                b = c(b, a)
        }
        names(b) = names(positive.values)
        
        # displays a sorted list of discriminative words (p<.05)
        words.preferred.by.primary.author = names(sort(b[b<0.05]))
        words.preferred = b[b<0.05]
        
        # avoided words
        b = NULL
        for(i in 1: length(negative.differences[,1])) {
                a = chisq.test(c(negative.differences[i,1],negative.differences[i,2]))$p.value
                b = c(b, a)
        }
        names(b) = names(negative.values)
        
        # displays sorted list of discriminative words (p<.05)
        words.avoided.by.primary.author = names(sort(b[b<0.05]))
        words.avoided = b[b<0.05]
        
        words.preferred.stats = NULL
        for (name in names(words.preferred)){
                stat = chisq.test(c(positive.differences[name,1],positive.differences[name,2]))$statistic
                words.preferred.stats = c(words.preferred.stats, stat)
        }
        names(words.preferred.stats) = names(words.preferred)
        words.preferred.stats = sort(words.preferred.stats, decreasing=T)
        
        words.avoided.stats = NULL
        for (name in names(words.avoided)){
                stat = chisq.test(c(negative.differences[name,1],negative.differences[name,2]))$statistic
                words.avoided.stats = c(words.avoided.stats, stat)
        }
        names(words.avoided.stats) = names(words.avoided)
        words.avoided.stats = sort(words.avoided.stats, decreasing=T)
    

        results = list(words.preferred.stats,words.avoided.stats)
        names(results) = c("words_preferred", "words_avoided")
        
return(results)
}

