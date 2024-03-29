% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Connection.R
\name{createCemConnection}
\alias{createCemConnection}
\title{Function creates a connection to}
\usage{
createCemConnection(
  apiUrl = "https://cem.ohdsi.org",
  connectionDetails = NULL,
  cemDatabaseSchema = NULL,
  sourceDatabaseSchema = NULL,
  vocabularyDatabaseSchema = NULL
)
}
\arguments{
\item{apiUrl}{url to cem connector hosted endpoint}

\item{connectionDetails}{DatabaseConnector connection details object for connection to db}

\item{cemDatabaseSchema}{schema for cem (if using database backend)}

\item{sourceDatabaseSchema}{- schema containing source info}

\item{vocabularyDatabaseSchema}{- schema for cem vocabulary}
}
\description{
Takes either connection details and CEM parameters and returns a backend object that can be used, transparently, to
interface either with a web API or database.

Factory pattern creation of abstract  CemBackend classes
}
