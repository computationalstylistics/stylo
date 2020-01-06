
# #################################################
# Function for graph auto-coloring; depending on
# the user's choice, it assigns colors or greyscale tones
# to matching strings of characters in texts' names
# (as a delimiter, the underscore character is used);
# alternatively, all the labels can be marked black. 
# Required argument: a vector of labels (text names)
# Optional argument: col="colors" || "greyscale" || "black"
# #################################################

assign.plot.colors <-
function(labels, col = "colors", opacity = 1) {
  if(col == "black") {
    vector.of.colors = rep("black", length(labels))
  } else {
    
    # generate unique list of cleaned labels
    labels_cleaned <- gsub("_.*","",labels)
    distinct_labels_cleaned <- unique(as.data.frame(labels_cleaned))
    
    # assign a color identifier to each label
    distinct_labels_cleaned$colors <- c(1:length(distinct_labels_cleaned$labels_cleaned))
    
    # join the list of of distinct label-color pairs to the full list of labels
    labels_cleaned <- merge(as.data.frame(labels_cleaned), distinct_labels_cleaned, by = 'labels_cleaned')
    
    color.numeric.values <- labels_cleaned$colors
    
  # define a vector of available colors, if an appropriate option was chosen
  if(col == "colors" && opacity >= 1) {
    available.colors = rep_len(c("red","green","blue","black","orange","purple",
      "darkgrey","brown","maroon4","mediumturquoise","gold4", "deepskyblue",
      "greenyellow","grey","chartreuse4", "khaki", "navy", "palevioletred",
      "darkolivegreen4", "chocolate4", "yellowgreen"), max(color.numeric.values))
    }
  if(col == "colors" && opacity < 1) {
    available.colors = rep_len(c(
      rgb(1,0,0, (opacity*0.8)), # red, opacity slightly tuned
      rgb(0.2,0.8,0, opacity), # green
      rgb(0,0,1, opacity), # blue
      rgb(0,0,0, opacity), # black
      rgb(1,0.7,0, opacity), # yellow/organge
      rgb(1,0,0.8, opacity), # purple but slightly different
      rgb(0.5,0.5,0.5, (opacity*0.8)) # dark grey
      ), max(color.numeric.values))
  }
  # define a vector of gray tones, instead of colors
  if(col == "greyscale") {
    number.of.colors.required = max(color.numeric.values)
    available.colors = gray(seq(0,0.7,0.7/(number.of.colors.required-1)), opacity)
  }
  # produce the final vector of colors (or gray tones)
  vector.of.colors = available.colors[c(color.numeric.values)]
  }
  # assigning names to the colors
  names(vector.of.colors) = labels
return(vector.of.colors)
}
