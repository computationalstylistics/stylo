
# #################################################
# Function for graph auto-coloring; depending on
# the user's choice, it assigns colors or grayscale tones
# to matching strings of characters in texts' names
# (as a delimiter, the underscore character is used);
# alternatively, all the labels can be marked black. 
# Required argument: a vector of text labels
# Optional argument: col="colors" || "grayscale" || "back"
# #################################################

assign.plot.colors <-
function(names.of.the.texts,col="colors") {
  if(col == "black") {
    colors.of.pca.graph = "black"
  } else {
    color.numeric.values = c(1)
    current.color = 1
    # a loop for matching the subsequent strings of chars
    for(w in 2:length(names.of.the.texts)) {
      # if two samples have the same id, don't change the color
      if(gsub("_.*","",names.of.the.texts)[w] %in%  
                       gsub("_.*","",names.of.the.texts[1:(w-1)]) == TRUE) {
        find.color = which(gsub("_.*","",names.of.the.texts) == 
                               gsub("_.*","",names.of.the.texts)[w])[1]
        current.color = color.numeric.values[find.color]
      # if the samples differ, assign the next color (actually, the color's id)
      } else {
        current.color = max(color.numeric.values) + 1
      }
    # append the recent color to the final vector of color values
    color.numeric.values = c(color.numeric.values, current.color)
    }
  # define a vector of available colors, if an appropriate option was chosen
  if(col == "colors") {
    available.colors = rep(c("red","green","blue","black","orange","purple",
      "darkgrey","brown","maroon4","mediumturquoise","gold4", "deepskyblue",
      "yellowgreen","grey","chartreuse4", "khaki", "navy", "palevioletred",
      "greenyellow", "darkolivegreen4", "chocolate4"),10)
    }
  # define a vector of gray tones, instead of colors
  if(col == "grayscale") {
    number.of.colors.required = max(color.numeric.values)
    available.colors = gray(seq(0,0.7,0.7/(number.of.colors.required-1)))
  }
  # produce the final vector of colors (or gray tones)
  colors.of.pca.graph = available.colors[c(color.numeric.values)]
  }
return(colors.of.pca.graph)
}
