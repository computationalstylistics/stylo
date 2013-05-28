
# #################################################
# FUNCTION: make.table.of.frequencies
# preparing a huge table with all the frequencies (> mwf.list.cutoff).
# Two arguments are required -- a vector with filenames
# and a specified variable (list) containing a corpus
# #################################################

# to implement: a sanity check if the generated table contains
# at least one row (variable)

# arguments: list of variables (wors, or features)
# a corpus

# names of samples: if exist, they should be retrieved from the list
# containing corpus. Otherwise, some generic numeric values should be 
# assigned. What about making it an optional argument?
# If missing, the names from the list will be used.
# If specified, a check will be needed if they match the number of samples


# some code for random sampling should be definitely excluded from
# this function


make.table.of.frequencies <- 
function(filenames,current.corpus) {
  freq.list.of.all.the.samples = c()
  freq.list.of.current.sample = c()
    for (file in filenames) {
    # loading the next sample from the list filenames.primary.set,
    current.sample = current.corpus[[file]]
    #
    # if random sampling was chosen, the text will be randomized and 
    # a sample of a given lenght will be excerpted
    if(random.sampling == TRUE) {
    current.sample = head(sample(current.sample,
                        replace = sampling.with.replacement), 
                        length.of.random.sample)
    }
    #
    #
    # preparing the frequency list of the current sample
    raw.freq = table(current.sample) * 100 / length(current.sample)
    # adjusting the frequency list to the main MFW list obtained above
    freq.list.of.current.sample = raw.freq[mfw.list.of.all]
    # taking the names (sc. words) from the main MFW list 
    names(freq.list.of.current.sample) = mfw.list.of.all
    # and sticking the current sample into the general frequency table
    freq.list.of.all.the.samples = 
               rbind(freq.list.of.all.the.samples, freq.list.of.current.sample)
    # a short message on the screen:
    cat(file, "\t", "excerpted successfully", "\n")
  }
  # adjusting names of the rows (=samples)
  rownames(freq.list.of.all.the.samples) = c(filenames)
# the result of the function
return(freq.list.of.all.the.samples)
}

