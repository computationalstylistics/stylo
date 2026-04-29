
# NOTE: this is set as colors in BCT, but this is strange!
# tip.color = colors.of.pca.graph

# NOTE:
# class = "stylo.results" # <- also "stylo_plot"?

# NOTE: change variables
# define `mds_flavor` # "labels", "points", "both"



plot.stylo.results = function(x, ...) {

    if(is.null(x$plot_obj) == TRUE) stop("Nothing to plot")
    plot(x$plot_obj, ...)

}

