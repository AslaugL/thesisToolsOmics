#' @title calculateFoldChange
#'
#' @description Calculates the fold change and log2 fold change between cases and controls.
#'
#' @param df Dataframe with a 'feature' column with feature names, a 'sample_id' column with sample ID's, a 'value' column with feature values for the samples, and a 'group' column splitting the samples into two groups (cases and controls).
#' @param type Calculate fold change from mean or median values? Default is 'median'.
#' @param case What are cases called in the 'group' column?
#' @param control What are controls called in the 'group' column?
#'
#' @return A dataframe with features and fold change/log2 fold change between cases and controls.
#'
#' @export

calculateFoldChange <- function(df, type = 'median', case, control){

  #Calculate fold change
  FC <- df %>%
    #Get the median for each compound for both case and control
    group_by(feature, group)

  #calculate FC either by median or mean
  if(type == 'median'){
    FC <- FC %>% summarise(num = median(value))
  } else if(type == 'mean'){
    FC <- FC %>% summarise(num = mean(value))
  } else {
    stop("Type must be either 'median' or 'mean'")
  }

  FC <- FC %>%
    ungroup() %>%

    #Arrange 'group' column so that cases are listed first, so that FC is calculated as cases/control
    arrange(factor(.$group, levels = c(case, control))) %>%

    #Calculate fold change
    pivot_wider(names_from = 'group',
                values_from = 'num') %>%

    mutate(fold_change = divideCols(., col1 = 2, col2 = 3)) %>%
    mutate(`log2(FC)` = log2(fold_change))

  df <- inner_join(df, FC)
}
