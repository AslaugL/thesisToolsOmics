#' Metabolomics data from ME/CFS patients and healthy controls.
#'
#' Taken from Prospective Biomarkers from Plasma Metabolomics of Myalgic Encephalomyelitis/Chronic Fatigue Syndrome Implicate Redox Imbalance in Disease Symptomatology. Metabolites. 2018 Dec 6;8(4):90. doi: 10.3390/metabo8040090.
#'
#' \itemize{
#'   \item feature. Name of metabolites.
#'   \item feature_anno. What type of feature is it?
#'   \item SUB PATHWAY. What pathway does this feature belong to?
#'   \item sample_id. Individual sample id's forr all participants in the study.
#'   \item value. Concentration value of the feature for the sample.
#'   \item group. Shows if samples are in the ME/CFS or control group}
#'
#' @docType data
#' @keywords datasets
#' @usage data(study2018)
#' @name study2018
#' @format A tibble with  40239 rows and 6 variables.
"study2018"
