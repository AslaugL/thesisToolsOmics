#' @title setOPLSmeta
#'
#' @description Define the samples and the group affiliation that separates samples before running ropls.
#'
#' @param df A tidy dataframe with sample_id and group column.
#'
#' @return A dataframe with the sample_id and group columns, with a united sample_id and group as rownames.
#'
#' @export

setOPLSmeta <- function(df) {

 #Meta
  meta <- df %>%
    select(sample_id, group) %>% #Get necessary columns
    unique() %>% #Remove duplicates

    #unite sample and group identifiers, keep group to separate
    unite('sample_group', c(sample_id, group), remove = FALSE) %>%
    column_to_rownames('sample_group') %>% #Turn sample_id into rownames for opls
    mutate_at('group', ~as.factor(.))

}
