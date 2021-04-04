#' @title getLipidType
#'
#' @description Extracts the type of lipid (DG, TG, LPC, AC etc) and potential tag (-OH, -DH etc) from lipid species names.
#'
#' @param df Dataframe with lipid species names in the first column.
#'
#' @return The dataframe with an additional Type column.
#'
#' @examples
#' data <- tibble(
#' 'feature' = c('AC(6:0)', 'AC(8:1)', 'AC(10:2)',
#'              'Cer(d18:0/16:0)', 'Cer(d18:0/24:1)', 'Cer(d18:1/24:1)',
#'              'DAG(18:0/18:1)', 'DAG(16:1/18:1)', 'DAG(18:2/18:3)'))
#' with_type_info <- data %>% getLipidType()
#'
#' @export

getLipidType <- function(df){

  with_type <- df %>%
    separate(col = 1, into = c('Type', 'Numbers'), sep = "-(?!\\w)| |\\(", remove = FALSE) %>% #Separate type of lipids from the numbers
    mutate_at('Numbers', ~str_replace(., '\\)', '')) %>% #Remove unnecessary characters

    #Get the tag such as -OH, -DH for some lipds
    separate(col = Numbers, into = c('Numbers', 'Tag'), sep = '-(?=\\w{1,2})') %>% #This creates an 'NA' in lipids with no tag
    unite(col = Type, c(Type, Tag), sep = '-') %>% #Add Tag to Type
    mutate_at('Type', ~str_replace(., '-NA', '')) %>% #Remove the NA tag

    #Remove unnecessary columns
    select(-Numbers)
  }
