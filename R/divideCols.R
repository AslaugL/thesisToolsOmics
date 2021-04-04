#' @title divideCols
#'
#' @description Divides values in one column with values in another column.
#'
#' @param df Dataframe with the columns.
#' @param col1 Numerator.
#' @param col2 Denominatr.
#'
#' @return A vector with the results.
#'
#' @export

divideCols <- function(df, col1, col2){
  df[[col1]] / df[[col2]]
}
