#' @title plotCarbonsDoubleBonds
#'
#' @description Creates a heatmap of logfold change of the lipids with carbon length on y-axis and double_bonds on x-axis
#'
#' @param df A dataframe with a log2(FC) column containing log2 fold change values of different lipid species, that has been run through countCarbonsDoubleBonds and getLipidType.
#'
#' @return A ggplot2 heatmap.
#'
#' @examples
#' data <- tibble(
#' 'feature' = c('AC(6:0)', 'AC(8:1)', 'AC(10:2)',
#'              'Cer(d18:0/16:0)', 'Cer(d18:0/24:1)', 'Cer(d18:1/24:1)',
#'              'DAG(18:0/18:1)', 'DAG(16:1/18:1)', 'DAG(18:2/18:3)'),
#' 'log2(FC)' = c(-2, 0.1, 1.5, 0.5, 0.7, -0.5, 1, 1, 1.2))
#' to_plot <- data %>% countCarbonsDoublebonds() %>% getLipidType()
#' plot <- to_plot %>% plotCarbonsDoublebonds()
#'
#' @export

plotCarbonsDoublebonds <- function(df) {

  #Temporary plot
  plot <- ggplot(df, aes(x = double_bonds, y = as.factor(carbons))) +
    geom_tile(aes(fill = `log2(FC)`)) +
    geom_text(aes(label = `log2(FC)`), color = 'white', size = 3, show.legend = FALSE) +

    #Facet by type of lipid
    facet_grid(rows = vars(Type),
               scales = 'free',
               space = 'free') +

    #Set color and axis scales
    scale_fill_continuous(type = 'viridis') +
    scale_x_continuous(breaks = min(df$double_bonds):max(df$double_bonds)) +

    #Add borders around the plot and have the names of the grid elements horizontal
    theme_light() +
    theme(strip.text.y = element_text(angle = 0),
          strip.text = element_text(color = 'black'),
          strip.background = element_rect(color = 'white', fill= 'white'),
          legend.position = 'right',
          legend.key.width = unit(0.5, 'cm')) + #Reduce width of legend key

    #Fix axis, legend title and keys
    labs(
      y = 'Total carbons',
      x = 'Double bonds'
    ) +
    guides(fill = guide_colorbar(title = 'Log2(FC)',
                                 label.position = 'left',
                                 label.hjust = 1))

}
