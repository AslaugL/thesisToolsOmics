% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLipidType.R
\name{getLipidType}
\alias{getLipidType}
\title{getLipidType}
\usage{
getLipidType(df)
}
\arguments{
\item{df}{Dataframe with lipid species names in the first column.}
}
\value{
The dataframe with an additional Type column.
}
\description{
Extracts the type of lipid (DG, TG, LPC, AC etc) and potential tag (-OH, -DH etc) from lipid species names.
}
\examples{
data <- tibble(
'feature' = c('AC(6:0)', 'AC(8:1)', 'AC(10:2)',
             'Cer(d18:0/16:0)', 'Cer(d18:0/24:1)', 'Cer(d18:1/24:1)',
             'DAG(18:0/18:1)', 'DAG(16:1/18:1)', 'DAG(18:2/18:3)'))
with_type_info <- data \%>\% getLipidType()

}
