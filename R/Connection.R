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

#' Function creates a connection to
#' @description
#' Takes either connection details and CEM parameters and returns a backend object that can be used, transparently, to
#' interface either with a web API or database.
#'
#' Factory pattern creation of abstract  CemBackend classes
#' @param apiUrl url to cem connector hosted endpoint
#' @param connectionDetails DatabaseConnector connection details object for connection to db
#' @param cemDatabaseSchema schema for cem (if using database backend)
#' @param sourceDatabaseSchema - schema containing source info
#' @param vocabularyDatabaseSchema - schema for cem vocabulary
#' @export
createCemConnection <- function(apiUrl = "https://cem.ohdsi.org",
                                connectionDetails = NULL,
                                cemDatabaseSchema = NULL,
                                sourceDatabaseSchema = NULL,
                                vocabularyDatabaseSchema = NULL) {
  if (is.null(connectionDetails) & !is.null(apiUrl)) {
    return(CemWebApiBackend$new(apiUrl = apiUrl))
  }

  return(CemDatabaseBackend$new(
    connectionDetails = connectionDetails,
    cemDatabaseSchema = cemDatabaseSchema,
    sourceDatabaseSchema = sourceDatabaseSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema
  ))
}
