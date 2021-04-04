#' @title doOPLS
#'
#' @description Run ropls.
#'
#' @param df A tidy dataframe with at least sample_id, group, feature and value column.
#'
#' @return A (r)opls OPLS object. Autoscaling is used on feature values.
#'
#' @export

doOPLS <- function(df){

  #Values to analyse
  values <- PCAprep(df)
  #Metadata
  meta <- setOPLSmeta(df)

  #Run (o)pls
  values.opls <- opls(values, meta$group,
                      predI = NA, orthoI = NA,
                      permI = 1000,
                      scaleC = 'standard') #Scale = how should ropls scale the data? 'Standard' is autoscaling.

  values.opls
}
