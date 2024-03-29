% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EvidenceExplorer.R
\name{launchCeExplorer}
\alias{launchCeExplorer}
\title{CE Explorer shiny application}
\usage{
launchCeExplorer(
  apiUrl = "https://cem.ohdsi.org",
  connectionDetails = NULL,
  usePooledConnection = TRUE,
  ...
)
}
\arguments{
\item{apiUrl}{string - url for CemConnector API or NULL}

\item{connectionDetails}{DatabaseConnector::connectionDetails instance for CEM}

\item{usePooledConnection}{- use pooled connections (database model only)}

\item{...}{param list paased to CemDatabaseBaackend$new}
}
\description{
Shiny App for exploring common evidence model
}
