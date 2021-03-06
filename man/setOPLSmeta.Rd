% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setOPLSmeta.R
\name{setOPLSmeta}
\alias{setOPLSmeta}
\title{setOPLSmeta}
\usage{
setOPLSmeta(df)
}
\arguments{
\item{df}{A tidy dataframe with sample_id and group column.}
}
\value{
A dataframe with the sample_id and group columns, with a united sample_id and group as rownames.
}
\description{
Define the samples and the group affiliation that separates samples before running ropls.
}
