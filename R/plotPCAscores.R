#' @title plotPCAscores
#'
#' @description Creates a PCA score plot using ggplot.
#'
#' @param df Dataframe with the principal components to be plotted in the first two columns, and a group column to color by.
#' @param prc A vector with the percentage the principal components contribute, to be used for x and y axis titles.
#'
#' @return A ggplot PCA score plot.
#'
#' @export

plotPCAscores <- function(df, prc){

  #Get the names of the principal component columns to be used as x and y axis
  pc_x <- names(df[1])
  pc_y <- names(df[2])

  #Color manual to color group by
  color_manual <- colorManuals(df = df, group = TRUE, feature_anno = FALSE)

  #Plot PCA colored by annotation column
  plot <- ggplot(df, aes(x = !!ensym(pc_x), y = !!ensym(pc_y), color = group)) +
    geom_point() +

    #Set color by annotation
    scale_color_manual(values = color_manual$sample_group,
                       guide = guide_legend(order = 1)) + #Set as first legend

  #Add title, correct labels
    labs(color = 'Group',
         x = prc[1],
         y = prc[2])

  #Add a dashed line at x = 0, y = 0 to show center of plot
  plot %>% addLinesToPlot()

}
