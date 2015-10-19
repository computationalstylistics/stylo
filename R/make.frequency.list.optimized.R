
# #################################################
# Function for generating a frequency list of words or other (linguistic)
# features. It basically counts the elements of a vector and returns a vector
# of these elements in descending order of frequency.
# Refer to help(make.frequency.list) for farther details.
# #################################################

make.frequency.list.optimized = function(data, 
                               value = FALSE,
                               head = NULL,
                               relative = TRUE) {
     
                                       
                                       
     #####################################
     # first, sanitize the input dataset
     
     # test if the dataset belongs to 'stylo.corpus' class
     if(class(data) == "stylo.corpus" | is.list(data) == TRUE) {
             # unlist, or make one long text out of the corpus samples
#             data = unlist(data)    
     # otherwise, test if the dataset is a vector
     } else if(is.vector(data) == FALSE) {        
             # whet it is not, produce an error message and stop
             stop("unable to make a list of frequencies")
     }
     
     # test if the dataset has at least two elements
     if(length(data) < 3) {
             stop("you try to measure frequencies of an empty vector!")
     }
     #####################################

     
     aggregate.values = c()
     aggregate.length = 0
     
for(k in 1: length(data)) {
#for(k in 1: 3) {
	message(sprintf("%7s", k), appendLF = FALSE) 
#	cat(k)#round(k/length(data), 3))

	current.values = sort(table(data[[k]]), decreasing = TRUE)
        if(length(current.values) > 10000) {
            current.values = current.values[1:10000]
        }
      current.length = length(data[[k]])
      current.names  = names(current.values)
        # for the first time, there is nothing to combine
        if(k == 1) {
            aggregate.values = current.values
        }
      aggregate.names = unique(c(current.names, names(aggregate.values)))
      aggregate.length = aggregate.length + current.length

 tmp.values1 = current.values[aggregate.names] / current.length
 tmp.values2 = aggregate.values[aggregate.names] / aggregate.length
 names(tmp.values1) = aggregate.names
 names(tmp.values2) = aggregate.names

 tmp.values = rbind(tmp.values1, tmp.values2)
 tmp.values[which(is.na(tmp.values))] = 0
 
 aggregate.values = sort(colMeans(tmp.values), decreasing = TRUE)
         if(length(aggregate.values) > 10000) {
            aggregate.values = aggregate.values[1:10000]
        }

        message("\b\b\b\b\b\b\b", appendLF = FALSE)
}

message("       ")


frequent.features = aggregate.values[1:5000]

#     #####################################     
#     # the dataset sanitized, let counting the features begin!     
#     frequent.features = sort(table(data), decreasing = TRUE)
#     #####################################
#
#     
#     # if relative frequencies were requested, they are normalized
#     if(relative == TRUE) {
#             frequent.features = frequent.features / length(data) * 100
#     }
#     
#     # additionally, one might limit the number of the most frequent features;
#     # this will return first n elements only (this is the argument 'head')
#     if(is.numeric(head) == TRUE) {
#             # sanitizing the indicated number
#             head = abs(round(head))
#             if(head == 0) head = 1
#             # cutting off the list
#             frequent.features = frequent.features[1:head]
#     }
#     
#     # in most cases, one needs just a list of features, without frequencies
     if(value == FALSE) {
             frequent.features = names(frequent.features)
     }
     
     
return(frequent.features)     
}

