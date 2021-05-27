# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of CEMConnector
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

#'@title
#'Load API
#'@description
#'Loads plumber API
#'
#'@returns Plumber router object
#'@export
loadApi <- function(connectionDetails,
                    cemSchema = Sys.getenv("CEM_DATABASE_SCHEMA"),
                    vocabularySchema = Sys.getenv("CEM_DATABASE_VOCAB_SCHEMA"),
                    sourceSchema = Sys.getenv("CEM_DATABASE_INFO_SCHEMA"),
                    pathToPlumberApi = system.file(file.path("api", "plumber.R"), package = "CEMConnector"),
                    envir = new.env(parent = .GlobalEnv)) {

  checkmate::assert_file_exists(pathToPlumberApi)

  envir$cemBackendApi <- CEMDatabaseBackend$new(connectionDetails,
                                                cemSchema = cemSchema,
                                                vocabularySchema = vocabularySchema,
                                                sourceSchema = sourceSchema,
                                                usePooledConnection = getOption("CEMConnector.UsePooledConnection", TRUE))


  plumb <- plumber::pr(pathToPlumberApi, envir = envir)
  plumb <- plumber::pr_hook(plumb, "exit", function() {
    # Close down database connections
    envir$cemBackendApi$finalize()
  })
}