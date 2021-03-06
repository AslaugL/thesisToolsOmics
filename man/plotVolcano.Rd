% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotVolcano.R
\name{plotVolcano}
\alias{plotVolcano}
\title{plotVolcano}
\usage{
plotVolcano(df, fold_change_threshold = 1.5, features = NULL)
}
\arguments{
\item{df}{Dataframe with a log2(FC) column with log2 fold change values, and a p.adj column with p-values.}

\item{fold_change_threshold}{Where should the vertical lines be drawn in the plot to show the threshold for over/underrepresentation? Default 1.5.}

\item{features}{Character string of features to be labelled, default NULL.}
}
\value{
A volcano ggplot, with upward triangles for upregulated features, downward triangles for downregulated features and circles for features below the threshold.
}
\description{
Creates vertical lines to separate the different fold change categories, lines are drawn with a default fold change of 1.5.
}
