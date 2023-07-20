# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of CemConnector
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @title
#' Load API
#' @description
#' Loads plumber API for functions
#' @param connectionDetails   DatabaseConnector connection details object
#' @param cemDatabaseSchema           schema where matrix summary and merged evidence are found
#' @param vocabularyDatabaseSchema    vocabulary schema on database
#' @param sourceDatabaseSchema        schema for info about the CEM
#' @param pathToPlumberApi    path to plumber script (default is package's)
#' @param envir               R environment
#' @param openApiSpecPath     path to openApi specification to use
#' @returns
#' Plumber router object
#' @importFrom plumber pr pr_hook
#' @import checkmate
#' @export
loadApi <- function(connectionDetails,
                    cemDatabaseSchema = Sys.getenv("CEM_DATABASE_SCHEMA"),
                    vocabularyDatabaseSchema = Sys.getenv("CEM_DATABASE_VOCAB_SCHEMA"),
                    sourceDatabaseSchema = Sys.getenv("CEM_DATABASE_INFO_SCHEMA"),
                    pathToPlumberApi = system.file(file.path("api", "plumber.R"), package = "CemConnector"),
                    envir = new.env(parent = .GlobalEnv),
                    openApiSpecPath = system.file(file.path("api", "cemconnector_openapi.yaml"), package = "CemConnector")) {
  checkmate::assert_file_exists(pathToPlumberApi)
  connectionPooling <- getOption("CemConnector.UsePooledConnection", TRUE)
  envir$cemBackendApi <- CemDatabaseBackend$new(connectionDetails,
    cemDatabaseSchema = cemDatabaseSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    sourceDatabaseSchema = sourceDatabaseSchema,
    usePooledConnection = connectionPooling
  )

  plumb <- plumber::pr(pathToPlumberApi, envir = envir)
  customOasSpecification <- yaml::read_yaml(openApiSpecPath)
  plumb <- plumber::pr_set_api_spec(plumb, customOasSpecification)

  plumb <- plumber::pr_hook(plumb, "exit", function() {
    # Close down database connections
    envir$cemBackendApi$finalize()
  })

  plumb
}


#' Deploy rsconnect API
#' @description
#' Calls out to rsconnect deployApi to deploy to a platform
#'
#' @inheritParams loadApi
#' @param envVars list of additional optional envrionment variables to set that are required by your system setup
deployEntrypoint <- function(appName,
                             deploymentFolder = file.path(tempfile(), appName),
                             cemDatabaseSchema = Sys.getenv("CEM_DATABASE_SCHEMA"),
                             vocabularyDatabaseSchema = Sys.getenv("CEM_DATABASE_VOCAB_SCHEMA"),
                             sourceDatabaseSchema = Sys.getenv("CEM_DATABASE_INFO_SCHEMA"),
                             pathToPlumberApi = system.file(file.path("api", "plumber.R"), package = "CemConnector"),
                             envVars = list()) {
  checkmate::assertList(envVars)
  # copy files to deployment folder
  envVars$CEM_DATABASE_SCHEMA <- cemDatabaseSchema
  envVars$CEM_DATABASE_VOCAB_SCHEMA <- vocabularyDatabaseSchema
  envVars$CEM_DATABASE_INFO_SCHEMA <- sourceDatabaseSchema


  if (!dir.exists(deploymentFolder)) {
    dir.create(deploymentFolder, recursive = TRUE)
  }

  deploymentCode <-
    rsconnect::deployAPI(deploymentFolder, appName = appName, envVars = envVars)
}
