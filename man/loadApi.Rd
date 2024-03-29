% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API.R
\name{loadApi}
\alias{loadApi}
\title{Load API}
\usage{
loadApi(
  connectionDetails,
  cemDatabaseSchema = Sys.getenv("CEM_DATABASE_SCHEMA"),
  vocabularyDatabaseSchema = Sys.getenv("CEM_DATABASE_VOCAB_SCHEMA"),
  sourceDatabaseSchema = Sys.getenv("CEM_DATABASE_INFO_SCHEMA"),
  pathToPlumberApi = system.file(file.path("api", "plumber.R"), package =
    "CemConnector"),
  envir = new.env(parent = .GlobalEnv),
  openApiSpecPath = system.file(file.path("api", "cemconnector_openapi.yaml"), package
    = "CemConnector")
)
}
\arguments{
\item{connectionDetails}{DatabaseConnector connection details object}

\item{cemDatabaseSchema}{schema where matrix summary and merged evidence are found}

\item{vocabularyDatabaseSchema}{vocabulary schema on database}

\item{sourceDatabaseSchema}{schema for info about the CEM}

\item{pathToPlumberApi}{path to plumber script (default is package's)}

\item{envir}{R environment}

\item{openApiSpecPath}{path to openApi specification to use}
}
\value{
Plumber router object
}
\description{
Loads plumber API for functions
}
