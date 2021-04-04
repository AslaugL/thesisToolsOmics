#' @title plotVolcano
#'
#' @description Creates vertical lines to separate the different fold change categories, lines are drawn with a default fold change of 1.5.
#'
#' @param df Dataframe with a log2(FC) column with log2 fold change values, and a p.adj column with p-values.
#' @param fold_change_threshold Where should the vertical lines be drawn in the plot to show the threshold for over/underrepresentation? Default 1.5.
#' @param features Character string of features to be labelled, default NULL.
#'
#' @return A volcano ggplot, with upward triangles for upregulated features, downward triangles for downregulated features and circles for features below the threshold.
#'
#' @export

plotVolcano <- function(df, fold_change_threshold = 1.5, features = NULL) {
  #Use ggplot2 to plot a volcano plot

  #Upward/downward triangle for over/underrepresented features, circle for 'below threshold'
  shapes = setNames(c('\u25B2', '\u25CF', '\u25BC'), c('Overrepresented', 'Below threshold', 'Underrepresented'))

  if('feature_anno' %in% names(df)){
    #Create a color manual for the feature annotations
    #Temporary color manual with all colors
    temp <- colorManuals(df = df, group = FALSE, feature_anno = TRUE)
    #Only select those feature_annotations that are in df
    color_manual <- list(
      'feature_anno' = temp$feature_anno[unique(as.character(df$feature_anno2))])

    temp <- ggplot(df, aes(`log2(FC)`, -log10(p.adj), color = feature_anno2, fill = feature_anno2, shape = threshold))}

    else{
      #Temporary color manual with all colors
      temp <- colorManuals(df = df, group = FALSE, feature_anno = TRUE)
      #Only select those feature_annotations that are in df
      color_manual <- list(
        'feature_anno' = temp$feature_anno[unique(as.character(df$threshold))])

      temp <- ggplot(df, aes(`log2(FC)`, -log10(p.adj), shape = threshold, color = threshold))
    }

  #Plot
   plot <- temp +

    #Add the scatters
    geom_point(alpha = 0.8, size = 2) +

    #Add lines to show where the significant threshold is
    geom_vline(xintercept = log2(fold_change_threshold), linetype = 'dashed', alpha = 0.5) +
    geom_vline(xintercept = log2(1/fold_change_threshold), linetype = 'dashed', alpha = 0.5) +
    geom_hline(yintercept = 1.3, linetype = 'dashed', alpha = 0.5) +

    #Change shapes and add color
    scale_shape_manual(values = shapes) +
    scale_color_manual(values = color_manual$feature_anno) +
    #Remove legend title
    theme(legend.title = element_blank()) +

    #Change labs
    labs(
      x = "log2 fold change",
      y = "-log10 adjusted p-value")

   if(is.character(features)){
     plot <- plot +
       geom_label_repel(data = filterFeatures(df = df, features = features),
                        aes(label = feature), #Only show labels for the features above/below the threshold
                        size = 2.5, box.padding = unit(0.2, "lines"), show.legend = FALSE) #Change size of labels and not show legend for this layer

   } else {
     plot <- plot
   }

}
