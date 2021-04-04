#' @title countCarbonsDoubleBonds
#'
#' @description Count the number of carbons and double bonds in lipid species.
#'
#' @param df Dataframe with lipid species names in the first column. Nomenclature must be xx:x for carbons:double_bonds.
#'
#' @return The dataframe with two additional columns 'carbons' and 'double_bonds'.
#'
#' @examples
#' data <- tibble(
#' 'feature' = c('AC(6:0)', 'AC(8:1)', 'AC(10:2)',
#'              'Cer(d18:0/16:0)', 'Cer(d18:0/24:1)', 'Cer(d18:1/24:1)',
#'              'DAG(18:0/18:1)', 'DAG(16:1/18:1)', 'DAG(18:2/18:3)'))
#' counted <- data %>% countCarbonsDoublebonds()
#'
#' @export

countCarbonsDoublebonds <- function(df){

  counts <- df %>%

    #Count carbons and double bonds
    mutate(temp_carbons = str_extract_all(pull(., var = 1), '\\d{1,2}(?=:)')) %>%
    mutate(temp_double_bonds = str_extract_all(pull(., var = 1), '(?<=:)\\d{1}')) %>%

    #If there are multiple numbers, it will create a list column that must be unnested before
    #being turned into numberics for summation
    unnest(cols = c(temp_carbons, temp_double_bonds)) %>%
    mutate_at(c('temp_carbons', 'temp_double_bonds'), ~as.numeric(as.character(.))) %>%
    group_by(pull(., var = 1)) %>%
    mutate(carbons = sum(temp_carbons),
           double_bonds = sum(temp_double_bonds)) %>%
    ungroup(.) %>%

    #remove temp columns and duplicates created by unnest
    select(-c(temp_carbons, temp_double_bonds, `pull(., var = 1)`))  %>%
    unique()
}
