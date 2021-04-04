#' @title FCthreshold
#'
#' @description Add a 'threshold' column to change the shape of datapoints in the volcano-plot.
#'
#' @param df Dataframe with a p.adj column from rstatix and a log2(FC) column with the log2 fold change values.
#' @param p_threshold Adjusted p.value threshold. Default 0.05.
#' @param FC_threshold log2 fold_change threshold, default 1.5.
#'
#' @return The dataframe with a threshold column.
#'
#' @export

FCthreshold <- function(df, p_threshold = 0.05, FC_threshold = 1.5){

  #Get the log2 of the threshold for fold_change to filter by
  fc <- log2(FC_threshold)

  #Add threshold column
  added_columns <- df %>%
    mutate(threshold = case_when(
      p.adj < p_threshold & `log2(FC)` > fc ~ 'Overrepresented',
      p.adj < p_threshold & `log2(FC)` < -fc ~ 'Underrepresented',
      TRUE ~ 'Below threshold'
    ))

    #Add a column to color by
    if('feature_anno' %in% names(df)){
      added_columns <- added_columns %>% mutate(feature_anno2 = case_when(
        threshold == 'Below threshold' ~ 'Below threshold',
        TRUE ~ as.character(feature_anno)
      ))
    }else{
      added_columns <- added_columns %>% mutate(feature_anno2 = case_when(
        threshold == 'Overrepresented' ~ 'Overrepresented',
        threshold == 'Underrepresented' ~ 'Underrepresented',
        threshold == 'Below threshold' ~ 'Below threshold'))
    }

  #Add 'Below threshold' at the end of the factor for prettier plots
  final <- added_columns %>%
    mutate_at(c('threshold', 'feature_anno2'), ~as.factor(.)) %>%
    mutate_at(c('threshold', 'feature_anno2'), ~fct_relevel(., 'Below threshold', after = Inf))

}

