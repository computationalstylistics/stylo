




stylo.network = function(mfw.min = 100, mfw.max = 1000, ...) {
#

    # if any command-line arguments have been passed by a user, they will
    # be stored on the following list and used to overwrite the defaults
    passed.arguments = list(...)

    # 'networkD3' is a package for using the D3.js library.
    # Since it is suggested (not required!) by 'stylo', one has to first
    # test if it is installed in the system:
    test.networkD3 = tryCatch(networkD3::as.radialNetwork(hclust(dist(cbind(c(1,2),c(3,4))))), 
                              error = function(e) NULL)
    if(is.null(test.networkD3) == TRUE) {
        stop("To use this function, you have to install the library 'networkD3',
              e.g. by typing install.packages('networkD3') in the console")
    }


    # now, launching stylo with consensus network options on:
    a = stylo(mfw.min = mfw.min, mfw.max = mfw.max, display.on.screen = FALSE, 
              analysis.type = "BCT", 
              network = TRUE, network.tables = "both", ...)

    # getting the nodes and the edges from the results produced by 'stylo':
    nodes.table = a$list.of.nodes
    edges.table = a$list.of.edges


    # Show Time!
    # this function opens a browser and produces an interactive force-directed
    # network out of the nodes and edges provided by 'stylo':
    results = networkD3::forceNetwork(Links = edges.table, Nodes = nodes.table, 
                   Source = "Source", Target = "Target", 
                   Value = "Weight", NodeID = "Label", 
                   Group = "Group", opacity = 0.8, zoom = TRUE, 
                   opacityNoHover = TRUE, fontFamily = "Helvetica", 
                   bounded = TRUE, charge = -100) 


# outputting the results
return(results)
}


