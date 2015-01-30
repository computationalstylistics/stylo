###############################################################
# Function for drawing polygons around clouds of points of I and II set: 
# a module as rough as can be (this is a pre-alpha version!)
# Argument: a data frame containing 3 columns: x, y, class
###############################################################

# this function is not exported, thus it is not visible for the user

draw.polygons = function(summary.zeta.scores) {
  for(current.subset in c("primary","secondary")) {
    # extracting the points from the I/II set
    current.cloud = (summary.zeta.scores[grep(current.subset,summary.zeta.scores[,3]),1:2])
    current.cloud = as.numeric(as.matrix(current.cloud))
    current.cloud = matrix(current.cloud,ncol=2)
    current.cloud = as.data.frame(current.cloud)
    colnames(current.cloud) = c("x","y")
    # extreme right point
    a1 = current.cloud[current.cloud[,1] == max(current.cloud[,1]),]
    # if there are more points of the same $x, then select max(a2$y)
    a1 = a1[a1$y == max(a1$y),]
    # just in case (theoretically, there still might be more that one point)
    a1 = a1[1,]
    # extreme left point
    a2 = current.cloud[current.cloud[,1] == min(current.cloud[,1]),]
    # if there are more points of the same $x, then select min(a2$y)
    a2 = a2[a2$y == min(a2$y),]
    # just in case (theoretically, there still might be more that one point)
    a2 = a2[1,]
    #
    #
    a.i = a1
    a.j = a2
    #
    # variable initialization
    vertexes.of.polygon = a1
    #
    #
    for(o in 1: length(current.cloud[,1]) ) {
      for(m in 1: length(current.cloud[,1]) ) {
        does.the.point.fit = a.j
        # equation for a line through (any) two given points
        # just in case: for drawing vertical lines
        if(a.i$x == a.j$x) {a.i$x = a.i$x + a.i$x *0.01}
        # steepnes with a safety parameter (to avoid dividing by 0) 
        steepness = (a.i$y-a.j$y)/((a.i$x-a.j$x) + 0.000001)
        offset = a.j$y - (steepness * a.j$x)
        current.line = steepness + offset
        #
        # fitting any better line
        for(n in 1: length(current.cloud[,1]) ) {
          current.point = current.cloud[n,]
          if(current.point$x * steepness + offset > 
                            current.point$y + current.point$y * .01) { 
            a.j = current.point
            break
            }
        } # <-- loop n
        #
        if(a.j$y == does.the.point.fit$y && a.j$x == does.the.point.fit$x) { 
          break
          }
      } # <-- loop m
      #
      vertexes.of.polygon = rbind(vertexes.of.polygon, a.j)
      #
      if(a.j$y == a2$y && a.j$x == a2$x) { 
        break
        }
      #
      # current a.i (that turned to be the optimal point) becomes a new vertex
      a.i = a.j
      # consequently, the end point should be reset
      a.j = a2
    } # <-- loop o
    #
    #### the same procedure applied to the II set
    #
    a.i = a2
    a.j = a1
    #
    for(o in 1: length(current.cloud[,1]) ) {
      for(m in 1: length(current.cloud[,1]) ) {
        does.the.point.fit = a.j
        # equation for a line through (any) two given points
        # just in case: for drawing vertical lines
        if(a.i$x == a.j$x) {a.i$x = a.i$x - a.i$x *0.01}
        # steepnes with a safety parameter (to avoid dividing by 0)
        steepness = (a.i$y-a.j$y)/((a.i$x-a.j$x) + 0.000001)
        offset = a.j$y - (steepness * a.j$x)
        current.line = steepness + offset
        #
        # fitting any better line
        for(n in 1: length(current.cloud[,1]) ) {
          current.point = current.cloud[n,]
          if(current.point$x * steepness + offset < 
                            current.point$y - current.point$y * .01) { 
            a.j = current.point
            break
            }
        } # <-- loop n
        #
        if(a.j$y == does.the.point.fit$y && a.j$x == does.the.point.fit$x) { 
          break
          }
      } # <-- loop m
      #
      vertexes.of.polygon = rbind(vertexes.of.polygon, a.j)
      #
      if(a.j$y == a1$y && a.j$x == a1$x) { 
        break
        }
      #
      # current a.i (that turned to be the optimal point) becomes a new vertex
      a.i = a.j
      # consequently, the end point should be reset
      a.j = a1
    } # <-- loop o
  #
  #
  # final variables' assignments
  if(current.subset == "primary") {
    polygon.primary = vertexes.of.polygon
    } else {
    polygon.secondary = vertexes.of.polygon
    }
  } # <-- loop current.subset
  #
  # drawing two polygons
  polygon(polygon.primary,col=rgb(0,0,0,0.1),border=2)
  polygon(polygon.secondary,col=rgb(0,0,0,0.1),border=3)
}

