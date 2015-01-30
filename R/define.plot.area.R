
# #################################################
# Function that finds out the coordinates
# of scatterplots; it computes the extreme x and y
# values, adds some margins, and optionally extends
# the top margin if a plot uses sample labels
# Required arguments: (1) a vector of x coordinates,
# (2) a vector of y coordinates, optionally with names;
# optional arguments: (3) additional margins (expressed 
# in % of actual plot area), (4) label offset (in %)
# #################################################

define.plot.area <-
function(x.coord, y.coord, xymargins = 2, v.offset = 0) {
  # get horizontal plotting area (reasonable margins added on both sides):
  # (1) how long are the extreme samples' names
  left.label.length = nchar(names(x.coord)[(order(x.coord)[1])])
  right.label.length = nchar(names(x.coord)[(order(x.coord,decreasing=T)[1])])
  # (2) checking if the sample labels really exist
    if(length(left.label.length) == 0) {
      left.label.length = 0}
    if(length(right.label.length) == 0) {
      right.label.length = 0}
  # (3) x axis expansion (0.5% for each character of the extreme samples' names)
  left.expansion = left.label.length * 0.005
  right.expansion = right.label.length * 0.005
  # (4) size of the x axis
  x.axis.size = abs(max(x.coord) - min(x.coord))
  # (5) finally, get both x coordinates
  min.x = min(x.coord) - (left.expansion + 0.01 * xymargins) * x.axis.size
  max.x = max(x.coord) + (right.expansion + 0.01 * xymargins) * x.axis.size 
  #
  # get vertical plotting area (plus optional top margin):
  # (1) size of the y axis
  y.axis.size = abs(max(y.coord) - min(y.coord))
  # (2) top margin (added to fit the samples' labels)
  top.offset = 0.005 * y.axis.size
  # (3) finally, get both y coordinates
  min.y = min(y.coord) - 0.01 * xymargins * y.axis.size
  max.y = max(y.coord) + (0.01 * v.offset + 0.01 * xymargins) * y.axis.size
  #
  plot.area = list(c(min.x, max.x), c(min.y, max.y))
return(plot.area)
}
