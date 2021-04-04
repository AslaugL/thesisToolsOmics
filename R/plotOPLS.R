#' @title plotOPLSscores
#'
#' @description Plot the OPLS principal component and first orthogonal component scores in ggplot2.
#'
#' @param df A 'getOPLSscores' result dataframe.
#'
#' @return A OPLS scores plot, colored by group status.
#'
#' @export

plotOPLSscores <- function(df){

  color_manual <- colorManuals(df = df, group = TRUE, feature_anno = FALSE)

  df <- df %>% mutate_at('group', ~as.factor(.))

  #Plot the opls data with a 95% ci confidence ellipse
  plot <- ggplot(df, aes(x = p1, y = o1, color = group)) +

    #Plot OPLS score, color datapoints by group
    geom_point(size = 2) +
    #Add 95% confidence ellipse
    stat_conf_ellipse(inherit.aes = FALSE, aes(x = p1, y = o1, color = group, fill = group), alpha = 0.1, geom = 'polygon') + #Add eliptical around values

    #Fix the legends
    guides(color = guide_legend(order = 1, override.aes = list(fill = NA, linetype = 'blank')), #Remove gray background and border of color key
           fill = FALSE,
           linetype = FALSE) + #No fill/linetype legend keys

  #Add Correct labels
    labs(
         color = 'Group',
         x = 'OPLS-DA predictive component',
         y = 'Orthogonal component') +
    #Set colors
    scale_color_manual(values = color_manual$sample_group)

  #Add lines to show origo
  plot %>% addLinesToPlot(.)

}

#' @title plotOPLSloadings
#'
#' @description Plot the OPLS principal component and first orthogonal component loadings in ggplot2.
#'
#' @param df A 'getOPLSloadings' result dataframe, with p1 and o1 in the second and third column respectively, and a feature_anno column to color features by.
#' @param cutoff Numerical value used to label all features with a loading >cutoff or <-cutoff. Default NULL.
#' @param features Character string of features to be labelled, default NULL.
#'
#' @return A OPLS loading plot.
#'
#' @export

plotOPLSloadings <- function(df, cutoff = NULL, features = NULL){

  #Get the names of the principal component columns to be used as x and y axis
  pc_x <- names(df[2])
  pc_y <- names(df[3])

  #If df has a feature_anno column, use that for coloring
  if('feature_anno' %in% names(df)){
    #Create a color manual for the feature annotations
    #Temporary color manual with all colors
    temp <- colorManuals(df = df, group = FALSE, feature_anno = TRUE)
    #Only select those feature_annotations that are in df
    color_manual <- list(
      'feature_anno' = temp$feature_anno[unique(as.character(df$feature_anno))])

    #Build plot
    temp <- ggplot(df, aes(x = !!ensym(pc_x), y = !!ensym(pc_y), color = feature_anno)) +
      #Set color scale
      scale_color_manual(values = color_manual$feature_anno)
  } else if (!'feature_anno' %in% names(df)) {
    #Build plot
    temp <- ggplot(df, aes(x = !!ensym(pc_x), y = !!ensym(pc_y)))
  }

   plot <- temp +
     geom_point() +

  #Set names of axis and fix color legend so it doesn't show letters
    labs(
      color = '',
      x = paste0('Loadings ', pc_x),
      y = paste0('Loadings ', pc_y)
    ) +
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

  #Add labels/names to the features above/below the threshold
  if(is.numeric(cutoff)){
    plot <- plot +
      geom_label_repel(data = df %>% filter(df[2] > cutoff | df[3] > cutoff | df[2] < -cutoff | df[3] < -cutoff),
                       aes(label = feature), #Only show labels for the features above/below the threshold
                       size = 2.5, box.padding = unit(0.2, "lines"), show.legend = FALSE) #Change size of labels and not show legend for this layer

  }else if(is.character(features)){
    plot <- plot +
      geom_label_repel(data = filterFeatures(df = df, features = features),
                       aes(label = feature), #Only show labels for the features above/below the threshold
                       size = 2.5, box.padding = unit(0.2, "lines"), show.legend = FALSE) #Change size of labels and not show legend for this layer

  } else if (is.null(cutoff) & is.null(features)){
    plot <- plot
  } else {
    stop('Cutoff must be either a numerical or NULL, feature must either be a character string of features found in the dataframe or NULL.')
  }

  plot %>% addLinesToPlot(.)

}

#' @title plotOPLSpermutations
#'
#' @description Plot the results from the permutation test run in (r)opls.
#'
#' @param df A 'getOPLSpermutations' result dataframe.
#'
#' @return A permutation plot.
#'
#' @export

plotOPLSpermutations <- function(df) {

  #Build plot
  plot <- ggplot(df, aes(x = sim, y = Value, color = Measurement)) +

    #Reduce alpha of the overlapping points
    geom_point(alpha = 0.7) +

    #Add a horizontal line to show the original R2 and Q2 values
    geom_hline(data = df %>% filter(sim == 1 & Measurement == 'R2Y'), aes(yintercept = Value)) +#, colour = color_manual$permutations[1]) +
    geom_hline(data = df %>% filter(sim == 1 & Measurement == 'Q2Y'), aes(yintercept = Value)) +#, colour = color_manual$permutations[2]) +

    #Add legend in lower left corner
    theme(legend.position = c(0.85, 0.2),
          legend.title = element_blank(),
          legend.spacing.y = unit(0.05, 'cm'),

          #Add a white background with black lines
          legend.background = element_rect(fill="white",
                                           size=0.3, linetype="solid",
                                           colour ="black")) +

    #Add title and correct axis text
    labs(
      color = 'Stat', #change legend key title
      x = 'Correlation coefficient between original and permuted data',
      y = 'R2Y and Q2Y'
    ) +

  #Change colors and shapes of points
  scale_color_manual(values = c('Q2Y' = '#44AA99', 'R2Y' = '#AA4499'))

}

#' @title plotOPLSscores
#'
#' @description Create white plot element with the summary statistics of ropls opls model to be added to other OPLS plots.
#'
#' @param df A 'getOPLSsummary' result dataframe.
#'
#' @return A white plot element with the summary statistics of ropls opls model.
#'
#' @export

plotOPLSsummary <- function(df){

  plot <- ggplot(df, aes(x = 1:length(combined), y = 0, label = combined)) +
    geom_text(size = 3.5) +
    theme_void() #Set theme with no axes or anything

}

#' @title plotOPLSsplot
#'
#' @description Creates an s-plot for the (r)opls model.
#'
#' @param df A 'getOPLScorrcov' result dataframe with a feature_anno column to color features.
#' @param cov_cutoff Any feature with a covariance score below this is colored gray.
#' @param corr_cutoff Any feature with a correlation score below this is colored gray
#' @param VIP_cutoff Any feature with a principal VIP score below this is colored gray.
#' @param cutoff_labels If TRUE, adds labels to features above threshold values, if false no features have labels.
#' @param features Character string of features to be labelled, default NULL.
#'
#' @return A white plot element with the summary statistics of ropls opls model.
#'
#' @export

plotOPLSsplot <- function(df, cov_cutoff = NULL, corr_cutoff = NULL, VIP_cutoff = NULL, cutoff_labels = FALSE, features = NULL) {

  #Turn every point below threshold to gray color, either by cov/corr or by VIP
  #If df already has a feature_anno column
  if('feature_anno' %in% names(df)){

    #Create temporary color manual of the original data frame, to be adapted after feature_anno has been mutated in the next steps
    temp <- colorManuals(df = df, group = FALSE, feature_anno = TRUE)

    df <- df %>% mutate_at('feature_anno', ~as.character(.))

    if(!is.null(corr_cutoff) & is.null(cov_cutoff)){
      df <- df %>%
        mutate(feature_anno = case_when(Correlation < corr_cutoff & Correlation > -corr_cutoff ~ 'Below threshold',
                                      TRUE ~ feature_anno))
      }
    else if(!is.null(cov_cutoff) & is.null(corr_cutoff)){
      df <- df %>%
        mutate(feature_anno = case_when(Covariance < cov_cutoff & Covariance > -cov_cutoff ~ 'Below threshold',
                                      TRUE ~ feature_anno))
    }
    else if(!is.null(cov_cutoff) & !is.null(corr_cutoff)){
      df <- df %>%
        mutate(feature_anno = case_when(Covariance < cov_cutoff & Covariance > -cov_cutoff ~ 'Below threshold',
                                      Correlation < corr_cutoff & Correlation > -corr_cutoff ~ 'Below threshold',
                                      TRUE ~ feature_anno))
    }
    else if (!is.null(VIP_cutoff)) {
      df <- df %>%
        mutate(feature_anno = case_when(VIP < VIP_cutoff & VIP > -VIP_cutoff ~ 'Below threshold',
                                      TRUE ~ feature_anno))
      }

    } else if(!'feature_anno' %in% names(df)){#If df does not have a feature_anno column, create one
      df <- df %>% mutate(feature_anno = '')

      if(!is.null(corr_cutoff) & is.null(cov_cutoff)){
        df <- df %>%
          mutate(feature_anno = case_when(Correlation < corr_cutoff & Correlation > -corr_cutoff ~ 'Below threshold',
                                          TRUE ~ 'Above threshold'))
      }
      else if(!is.null(cov_cutoff) & is.null(corr_cutoff)){
        df <- df %>%
          mutate(feature_anno = case_when(Covariance < cov_cutoff & Covariance > -cov_cutoff ~ 'Below threshold',
                                          TRUE ~ 'Above threshold'))
      }
      else if(!is.null(cov_cutoff) & !is.null(corr_cutoff)){
        df <- df %>%
          mutate(feature_anno = case_when(Covariance < cov_cutoff & Covariance > -cov_cutoff ~ 'Below threshold',
                                          Correlation < corr_cutoff & Correlation > -corr_cutoff ~ 'Below threshold',
                                          TRUE ~ 'Above threshold'))
      }
      else if (!is.null(VIP_cutoff)) {
        df <- df %>%
          mutate(feature_anno = case_when(VIP < VIP_cutoff & VIP > -VIP_cutoff ~ 'Below threshold',
                                          TRUE ~ 'Above threshold'))
      }
    }

  #If feature_anno has other variables than '', turn into factor and relevel so 'Below threshold' is the last, and set the colors for the feature annotations
  if('' %in% df$feature_anno){
    plot <- ggplot(df, aes(x = Covariance, y = Correlation))
  }
  else{
    df <- df %>%
      mutate_at('feature_anno', ~as.factor(.)) %>%
      mutate_at(., 'feature_anno', ~fct_relevel(., 'Below threshold', after = Inf))

    #Create final color manual, extract only those feature annotations left in the dataframe
    color_manual <- list(
      'feature_anno' = temp$feature_anno[unique(as.character(df$feature_anno))])

    #Plot
    plot <- ggplot(df, aes(x = Covariance, y = Correlation, color = feature_anno)) +
      #set colors
      scale_color_manual(values = color_manual$feature_anno) +
      #Increase size of color keys
      guides(color = guide_legend(override.aes = list(size = 4)))
  }

   plot <- plot +
    geom_point() +

    #Set axis labs
    labs(
      color = '',
      x = 'Covariance with group label',
      y = 'Correlation with group label'
    )

  #Add labels to important points or not
  if(isTRUE(cutoff_labels)){
    plot <- plot + geom_label_repel(data = df %>% filter(feature_anno != 'Below threshold'), aes(label = feature), #Only show labels for the features above/below the threshold
                                    size = 2, box.padding = unit(0.2, "lines"), show.legend = FALSE) + #Change size of labels and not show legend for this layer)
      labs(
        color = '')}else{plot <- plot}

   if(is.character(features)){
     plot <- plot +
       geom_label_repel(data = filterFeatures(df = df, features = features) %>% unique(),
                        aes(label = feature), #Only show labels for the features above/below the threshold
                        size = 2.5, box.padding = unit(0.2, "lines"), show.legend = FALSE) #Change size of labels and not show legend for this layer
   }
  else{
    plot <- plot
  }

  plot %>% addLinesToPlot()

}

#' @title plotAllOPLS
#'
#' @description Plots an OPLS score plot, loading plot and s-plot.
#'
#' @param opls_object A ropls object.
#' @param ord_df The original df used to doOPLS.
#' @param meta Should setMetaData be used to add metadata to the plots? Default TRUE.
#' @param addLabels If TRUE, adds labels to features above threshold values, if false no features have labels.
#' @param cov_cutoff Any feature with a covariance score below this is colored gray.
#' @param corr_cutoff Any feature with a correlation score below this is colored gray
#' @param VIP_cutoff Any feature with a principal VIP score below this is colored gray.
#'
#' @return A white plot element with the summary statistics of ropls opls model.
#'
#' @export

plotAllOPLS <- function(opls_object, org_df, metadata = TRUE, addLabels = FALSE, cov_cutoff = NULL, corr_cutoff = NULL, VIP_cutoff = NULL) {
  #opls_object = ropls results, separator = the groups used running ropls

  if(isTRUE(metadata)){
  meta <- setMetaData(df = org_df)

    #Get the scores, loadings, covariance/correlations, permutations and model summary
    scores <- getOPLSscores(opls_object = opls_object)
    loadings <- getOPLSloadings(opls_object = opls_object) %>% inner_join(meta$features)
    permutations <- getOPLSpermutations(opls_object = opls_object)
    corrcov <- getOPLScorrcov(opls_object = opls_object, org_df = org_df) %>% inner_join(meta$features)
    summary <- getOPLSsummary(opls_object = opls_object)

  }
  else if(isFALSE(metadata)){
    #Get the scores, loadings, covariance/correlations, permutations and model summary
    scores <- getOPLSscores(opls_object = opls_object)
    loadings <- getOPLSloadings(opls_object = opls_object)
    permutations <- getOPLSpermutations(opls_object = opls_object)
    corrcov <- getOPLScorrcov(opls_object = opls_object, org_df = org_df)
    summary <- getOPLSsummary(opls_object = opls_object)
  }

  #Plot
  scores_plot <- plotOPLSscores(scores) %>%
    changeGGplotTxtSize()
  loadings_plot <- plotOPLSloadings(loadings) %>%
    changeGGplotTxtSize()
  permutations_plot <- plotOPLSpermutations(permutations) %>%
    changeGGplotTxtSize()
  splot <- plotOPLSsplot(corrcov, cov_cutoff = cov_cutoff, corr_cutoff = corr_cutoff, VIP_cutoff = VIP_cutoff) %>%
    changeGGplotTxtSize()
  summary_plot <- plotOPLSsummary(summary)

  #Plot scores and loadings together first
  scores_loadings <- plot_grid(

    #Score plot
    scores_plot + theme(

      #Set the legend of the OPLS-score plot in the lower left inner corner
      legend.position = c(.27, .07),
      legend.box = 'vertical',
      legend.direction = 'horizontal',
      legend.margin = margin(-7, -6, 0, 0),
      legend.spacing.y = unit(0.07, 'cm'),

      #Add a white background with black lines
      legend.background = element_rect(color="white")),


    #Loading plot
    loadings_plot + theme(legend.position = 'none'),
    rel_widths= c(1,1),
    labels = 'AUTO',
    axis = 'l',
    align = 'hv',
    ncol = 2)

  #Add s-plot and summary on the bottom
  sl_splot <- plot_grid(scores_loadings,

                        #Fix the legend position
                        splot + theme(
                          legend.box.spacing = unit(0.2, 'cm')
                        ),
                        summary_plot,
                        nrow = 3,
                        labels = c('', 'C'),
                        axis = 'l',
                        rel_heights = c(1,2,0.16))

}
