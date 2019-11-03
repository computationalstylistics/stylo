
plot.sample.size = function(x, 
                            target = NULL, 
                            variable = "diversity",
                            trendline = TRUE,
                            observations = FALSE,
                            grayscale = FALSE,
                            legend = TRUE, 
                            legend_pos = "bottomright",
                            ...) {
    #
    
    if(options()$warn < 0) {
        message = function() {NULL}
    }
    
    
    # fist, capturing any additional parameters passed by the user
    add.args = list(...)

    
    
    diversity = x$diversity.scores
    accuracy = x$accuracy.scores
    
    
    
    
    if(length(target) > 1) {
        stop("The argument 'target' cannot have more than one value at a time!")
    }
    
    if(is.numeric(target) == TRUE ) {
        if((target <= length(x$test.texts)) == TRUE ) {
            target = x$test.texts[target]
            message(target, " -- plotting sample size properties")
        } else {
            stop("The argument 'target' cannot be greater than the number of available texts!")
        }
    } else if(is.character(target) == TRUE ) {
        if( (target %in% x$test.texts) == TRUE ) {
            message(target, " -- plotting sample size properties")
        } else {
            stop("Target text ", target, " doesn't exist. Use one of the following texts: \n", 
                 paste("    ", x$test.texts, "\n", sep = ""))
        }
    } else {
        target = NULL
    }
    
    
    if(is.null(target) & length(x$test.texts) == 1 ) {
        target = x$test.texts[1]
        message(target, " -- plotting sample size properties")
    }
    
    
    if(is.null(target) & length(x$test.texts) > 1) {
        message("\nResults for more than one text available; these are as follows: \n", 
            paste("    ", x$test.texts, "\n", sep = ""),
            "The first one will be used to generate the plot. If you want to use a different\n",
            "one, please specify it via the argument 'target', e.g. \n    plot(x, target = \"",
            x$test.texts[2], "\")", "\n    plot(x, target = 2)\n", 
            "(Both ways are equivalent, where a numeric value represents the n-th text).")
        message("")
        target = x$test.texts[1]
    }
    
    
    if(variable == "diversity"){
        dataset = diversity[[target]]
        main_title = paste(target, " (Simpson's diversity)", sep = "")
        ylabel = "Simpson's index"
    } else if(variable == "accuracy"){
        dataset = accuracy[[target]]
        main_title = paste(target, " (classification accuracy)", sep = "")
        ylabel = "classification accuracy"
#    } else if(variable == "dispersion"){
#        dataset = accuracy[[target]]
#        main_title = paste(target, " (classification accuracy)", sep = "")
#        ylabel = "classification accuracy"
    } else {
        warning("Unknown value of the argument 'variable'. Performing 'variable = \"diversity\"'.")    
    }
    
    
    
    no_of_variables = length(dataset[,1])
    
    if(variable == "diversity") {
        plot_color = hsv(0.9, 1, seq(1, 0.3, length.out = no_of_variables))
    }
    if(variable == "accuracy") {
        plot_color = hsv(0.6, 1, seq(1, 0.3, length.out = no_of_variables))
    }
    if(variable == "dispersion") {
        plot_color = hsv(0.3, 1, seq(1, 0.3, length.out = no_of_variables))
    }
    if(grayscale == TRUE) {
        plot_color = gray.colors(no_of_variables, start = 0.7, end = 0)
    }
    
    
    
    plot(dataset[1,] ~ as.numeric(colnames(dataset)),
         ylim = c(0,1), 
         ylab = ylabel, 
         xlab = "sample size", 
         main = main_title, 
         type = "n", ...)
    
    
    for(a in 1:no_of_variables) {
        if(observations == TRUE) {
            points(dataset[a,] ~ as.numeric(colnames(dataset)), col = plot_color[a], pch = 1)
        }
        if(trendline == TRUE) {
            lines(lowess(dataset[a,], f = 1/5)$y ~ as.numeric(colnames(dataset)), col = plot_color[a], lwd = 3)
        }
    }
# maybe a dot rather than a line in the legend? (pch = 20 rather than lwd = 3)
    if(legend == TRUE) {
        legend(x = legend_pos, legend = rownames(dataset), bty = "n", text.col = plot_color, col = plot_color, pch = 20)
    }
    
    
}




#texts = xxx$test.texts
#for(i in texts) {
#    file_png = paste(i, ".png", sep = "")
#    png(filename = file_png)
#    plot(xxx, variable = "diversity", target = i)
#    dev.off()
#}

