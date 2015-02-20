
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
    color.numeric.values = c(1)
    current.color = 1
    # a loop for matching the subsequent strings of chars
    for(w in 2:length(labels)) {
      # if two samples have the same id, don't change the color
      if(gsub("_.*","",labels)[w] %in%  
                       gsub("_.*","",labels[1:(w-1)]) == TRUE) {
        find.color = which(gsub("_.*","",labels) == 
                               gsub("_.*","",labels)[w])[1]
        current.color = color.numeric.values[find.color]
      # if the samples differ, assign the next color (actually, the color's id)
      } else {
        current.color = max(color.numeric.values) + 1
      }
    # append the recent color to the final vector of color values
    color.numeric.values = c(color.numeric.values, current.color)
    }
  # define a vector of available colors, if an appropriate option was chosen
  if(col == "colors" && opacity >= 1) {
    available.colors = rep(c("red","green","blue","black","orange","purple",
      "darkgrey","brown","maroon4","mediumturquoise","gold4", "deepskyblue",
      "greenyellow","grey","chartreuse4", "khaki", "navy", "palevioletred",
      "darkolivegreen4", "chocolate4", "yellowgreen"),10)
    }
  if(col == "colors" && opacity < 1) {
    available.colors = rep(c(
      rgb(1,0,0, (opacity*0.8)), # red, opacity slightly tuned
      rgb(0.2,0.8,0, opacity), # green
      rgb(0,0,1, opacity), # blue
      rgb(0,0,0, opacity), # black
      rgb(1,0.7,0, opacity), # yellow/organge
      rgb(1,0,0.8, opacity), # purple but slightly different
      rgb(0.5,0.5,0.5, (opacity*0.8)) # dark grey
      ),40)
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
