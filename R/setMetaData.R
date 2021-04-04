#' @title setMetaData
#' @description Create a list of metadata for samples and features.
#' @param df The dataframe to collect metadata from.
#' @param sample_col The columns to select as metadata for samples, default is 'group'. Set as FALSE if not in use.
#' @param feature_col The columns to select as metadata for features, default is 'feature_anno'.Set as FALSE if not in use.
#' @return A list with sample and/or feature metadata.
#' @export

setMetaData <- function(df, sample_col = 'group', feature_col = 'feature_anno'){

  if(isFALSE(feature_col)){
    meta <- list(
      'samples' = df %>% select(c('sample_id', sample_col)) %>% unique()
      )}
  else if(isFALSE(sample_col)){
      meta <- list(
        'features' = df %>% select(c(feature, feature_col)) %>% unique()
      )
      } else {
      meta <- list(
        'samples' = df %>% select(c('sample_id', sample_col)) %>% unique(),
        'features' = df %>% select(c(feature, feature_col)) %>% unique()
      )
    }
  }

