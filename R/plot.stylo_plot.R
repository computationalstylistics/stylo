







plot.stylo_plot = function(x, ...) {



    if (x$plot_type == "dendrogram") {

        # define margins
        if(x$dendro_horizontal) {
            dendr_margins =  c(5,4,4,8)+0.1
        } else {
            dendr_margins = c(8,5,4,4)+0.1 
        }
        # set the margins
        par(mar = dendr_margins)

        plot(as.dendrogram(x$plot_data), 
            main = x$title, 
            horiz = x$dendro_horizontal, ...)
        # add some tweaks if vertical layout is chosen
        if (x$dendro_horizontal == TRUE) {
            title(sub = x$subtitle)
        } else {
            title(sub = x$subtitle, outer = TRUE, line = -1)
        }

        # restore default settings
        par(mar = c(5, 4, 4, 2) + 0.1)

    }



    if (x$plot_type == "BCT") {

        plot(x$plot_data, 
            type = "u", 
            font = 1, 
            lab4ut = "axial",
            tip.color = x$plot_colors,
            main = x$title,
            sub = x$subtitle, ...)

    }


    if (x$plot_type == "MDS") {

        # define the coordinates of the points
        xy_coord = x$plot_data$points[, 1:2]
        x_coord = x$plot_data$points[, 1]
        y_coord = x$plot_data$points[, 2]

        plot_area = define.plot.area(x_coord, y_coord,
            xymargins = x$add_to_margins,
            v.offset = x$label_offset)

        # start with an empty plot
        plot(xy_coord, type = "n",
            ylab = "", xlab = "",
            xlim = plot_area[[1]], ylim = plot_area[[2]],
            main = x$title,
            sub = x$subtitle, ...)

        # add points
        if (x$mds_flavor == "points" || x$mds_flavor == "both") {
            points(xy_coord, 
                col = x$plot_colors,
                lwd = x$line_width, ...)
        }

        # add a small offset between the labels and the points
        if (x$mds_flavor == "both") {
            label_coord = cbind(x_coord, (y_coord + (0.01 * x$label_offset * 
                abs(max(y_coord) - min(y_coord)))))
        } else {
            label_coord = xy_coord
        }

        # add text labels to the plot
        if (x$mds_flavor == "labels" || x$mds_flavor == "both") {
            text(label_coord, rownames(label_coord), col = x$plot_colors)
        }

        # add axes and a box around the whole plot
        axis(1, lwd = x$line_width)
        axis(2, lwd = x$line_width)
        box(lwd = x$line_width)

    }


    # for each type of PCA, define x and y labels
    if (x$plot_type == "PCA") {

        # get the variation explained by the PCs:
        expl_var = round(( 
            (x$plot_data$sdev ^ 2) / sum(x$plot_data$sdev ^ 2) 
            * 100), 1)
        PC1_lab = paste("PC1 (", expl_var[1], "%)", sep="")
        PC2_lab = paste("PC2 (", expl_var[2], "%)", sep="")

    } else {
        # if not PCA, set an empty value to x$pca_flavor
        # so that it can be used to silence other methods
        # in the conditions that follow
        x$pca_flavor = "not_PCA_anyway" 
    }


    # if 'loadings', plot samples and loadings in the same plot
    if (x$plot_type == "PCA" && x$pca_flavor == "loadings") {

        biplot(x$plot_data,
            col = c("grey70", "black"),
            cex = c(0.7, 1), 
            xlab = "PC1_lab",
            ylab = PC2_lab,
            main = x$title, #paste(x$title, "\n\n", sep=""),
            sub = x$subtitle, #paste(PC1_lab, "\n", x$subtitle, sep = ""), 
            var.axes = FALSE, ...)

    }


    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 'technical' is not ported yet...
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #
    if (x$plot_type == "PCA" && x$pca_flavor == "technical") {

        plot(x = 1, xlim = c(0, 1), ylim = c(0, 1), type = "n")
        text(c(0.5, 0.5), labels = "Coming soon...", pos = 2, col = "red")

    }
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    # all 'classic' favors: points, labels and both
    if (x$pca_flavor == "points" || x$pca_flavor == "both" || x$pca_flavor == "labels") {

        plot_area = define.plot.area(x$plot_data$x[, 1], 
                    x$plot_data$x[, 2],
                    xymargins = x$add_to_margins,
                    v.offset = x$label_offset)

        # start an empty plot
        plot(x$plot_data$x[, 1:2],
            type = "n",
            xlim = plot_area[[1]], 
            ylim = plot_area[[2]],
            xlab = PC1_lab, 
            ylab = PC2_lab,
            main = x$title, 
            sub = x$subtitle, #paste(PC1_lab, "\n", x$subtitle),
            col = x$plot_colors, ...)

    }


    # if 'labels' chosen, add text labels to the empty plot
    if (x$pca_flavor == "labels") {

        text(x$plot_data$x[, 1:2], 
            rownames(x$plot_data$x[, 1:2]), 
            col = x$plot_colors)

    }


    # for 'points' and 'both', add points to the plot
    if (x$pca_flavor == "points" || x$pca_flavor == "both") {

        points(x$plot_data$x[, 1:2],
            col = x$plot_colors, ...)

    }


    # finally, add text labels (with an offset) to the plot
    if (x$pca_flavor == "both") {

        label_coord = cbind(x$plot_data$x[, 1], (x$plot_data$x[, 2] + 
            (0.01 * x$label_offset *
            abs(max(pca.results$x[,2]) - min(pca.results$x[,2])))))
        text(label_coord, 
            rownames(x$plot_data$x[, 1:2]), 
            col = x$plot_colors)

    }

}






