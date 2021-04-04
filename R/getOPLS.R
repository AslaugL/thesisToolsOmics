#' @title getOPLSscores
#'
#' @description Extract the principal and orthogonal scores from a (r)opls object.
#'
#' @param opls_object A ropls object.
#'
#' @return A dataframe with the principal and orthogonal scores for each sample.
#'
#' @export

getOPLSscores <- function(opls_object) {
  #Get the scores from the ropls results

  df <- data.frame(opls_object@scoreMN, opls_object@orthoScoreMN) %>%

    #Get the normal subject IDs back and get metadata
    rownames_to_column('sample_group') %>% #Get the united sample_group column back
    separate(sample_group, c('sample_id', 'group'), sep = '_') #split into sample_id and group

}

#' @title getOPLSloadings
#'
#' @description Extract feature loadings from a (r)opls object.
#'
#' @param opls_object A ropls object.
#'
#' @return A dataframe with the principal and orthogonal loadings for each feature
#'
#' @export

getOPLSloadings <- function(opls_object) {
  #Get the OPLS loadings results and add type metadata

  loadings <- data.frame(opls_object@loadingMN, opls_object@orthoLoadingMN) %>%
    rownames_to_column('feature') #%>% #Get the feature names back

    #Add type metadata
    #inner_join(., tidy_data %>% select(feature, feature_anno) %>% unique()) %>%

    #Create abbreviations for type to use less space in plot
    #mutate_at(., 'feature_anno', ~as.factor(.))
}

#' @title getOPLSpermutations
#'
#' @description Extract permutation results from a (r)opls object.
#'
#' @param opls_object A ropls object.
#'
#' @return A dataframe with the R2Y, Q2Y values and simulation number.
#'
#' @export

getOPLSpermutations <- function(opls_object){
  #Get the data needed to create a plot of the permutations done when building the opls model
  permutation_data <- data.frame(opls_object@suppLs$permMN) %>%

    #rename relevant columns
    rename(R2Y = R2Y.cum., Q2Y = Q2.cum.) %>%

    #Select columns relevant to plot and turn into a tidy/long df
    select(R2Y, Q2Y, sim) %>%
    pivot_longer(.,
                 cols = c(R2Y, Q2Y),
                 names_to = 'Measurement',
                 values_to = 'Value')
}

#' @title getOPLSsummary
#'
#' @description Extract summary statistics from a (r)opls object.
#'
#' @param opls_object A ropls object.
#'
#' @return A dataframe with the summary statistics provided by (r)opls.
#'
#' @export

getOPLSsummary <- function(opls_object){
  #Summary statistics from ropls

  summary <- data.frame(opls_object@summaryDF) %>%
    pivot_longer(everything(),
                 names_to = 'statistics',
                 values_to = 'value') %>%

    #Combine the statistics and values to later add to plots
    mutate(combined = paste0(statistics, ':\n', value)) %>%

    #Clean up
    mutate_at('combined', ~str_replace_all(., '.cum.', ' cum'))
}

#' @title getOPLSvip
#'
#' @description Extract feature principal component VIPs from a (r)opls object.
#'
#' @param opls_object A ropls object.
#'
#' @return A dataframe with the feature and their principal VIP scores from a (r)opls object.
#'
#' @export

getOPLSvip <- function(opls_object) {
  #Get the oplsVIP scores

  #Get VIP and the correct colnames
  temp <- as.data.frame(opls_object@vipVn)
  colnames(temp) <- 'VIP'

  pVIP <- temp %>%
    rownames_to_column('feature') #%>%
    #inner_join(., meta$features)
}

#' @title getOPLSorthovip
#'
#' @description Extract feature and their orthogonal VIP scores from a (r)opls object.
#'
#' @param opls_object A ropls object.
#'
#' @return A dataframe with the feature and their orthogonal VIP score.
#'
#' @export

getOPLSorthovip <- function(opls_object){
  #Get the opls ortho VIP scores by

  #Get VIP and the correct colnames
  temp <- as.data.frame(opls_object@orthoVipVn)
  colnames(temp) <- 'VIP'

  oVIP <- temp %>%
    rownames_to_column('feature') %>%
    inner_join(., tidy_data %>% select(feature, feature_anno))
}

#' @title getOPLScorrcov
#'
#' @description Get the covariance and correlations between the scores and the features used to calculate them.
#'
#' @param opls_object A ropls object.
#' @param ord_df The original df used to doOPLS.
#'
#' @return A dataframe with the correlations and covariance between opls principal score and the different features.
#'
#' @export

getOPLScorrcov <- function(opls_object, org_df) {
  #

  #Dataframe with samples in rows and features in columns, data should be scaled
  opls_df <- PCAprep(org_df) %>% scale(.)

  #Scores
  scores <- getOPLSscores(opls_object = opls_object) %>%
    select(p1)

  #Correlation, use psych's corr.test and tibble to keep the names
  temp <- corr.test(scores, opls_df, ci = FALSE, method = 'kendall')

  corr <- as_tibble(temp$r) %>%
    pivot_longer(everything(),
                 names_to = 'feature',
                 values_to = 'Correlation')

  #Covariance
  cov <- data.frame(cov(scores, opls_df)) %>%
    pivot_longer(everything(),
                 names_to = 'feature2',
                 values_to = 'Covariance')

  #Get original loadings with feature names
  temp <- getOPLSloadings(opls_object = opls_object) %>%
    select(p1, feature)

  #Turn into one df
  corrcov <- bind_cols(corr, cov) %>%
    select(-feature2) %>%
    #inner_join(., meta$features) %>% #Add feature metadata

    #Add VIPvn to color things by later
    inner_join(getOPLSvip(opls_object = opls_object)) %>%

    #Add original score back
    inner_join(., temp)
}
