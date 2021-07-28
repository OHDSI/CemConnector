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

#' @importFrom DBI dbIsValid
#' @importFrom SqlRender render translate
ConnectionHandler <- R6::R6Class(
  "ConnectionHandler",
  public = list(
    connectionDetails = NULL,
    con = NULL,
    isActive = FALSE,
    initialize = function(connectionDetails) {
      checkmate::assert_class(connectionDetails, "connectionDetails")
      self$connectionDetails <- connectionDetails
      self$initConnection()
    },

    renderTranslateSql = function(query, ...) {
      sql <- SqlRender::render(sql = query, ...)
      SqlRender::translate(sql, targetDialect = self$connectionDetails$dbms)
    },

    initConnection = function() {
      if (self$isActive) {
        warning("Closing existing connection")
        self$closeConnection()
      }
      self$con <- DatabaseConnector::connect(connectionDetails = self$connectionDetails)
      self$isActive <- TRUE
    },

    getConnection = function() {
      return(self$con)
    },

    closeConnection = function() {
      if (DBI::dbIsValid(dbObj = self$con)) {
        DatabaseConnector::disconnect(self$con)
      }
      self$isActive <- FALSE
    },

    finalize = function() {
      if (self$isActive & DBI::dbIsValid(dbObj = self$con)) {
        self$closeConnection()
      }
    },

    queryDb = function(query, snakeCaseToCamelCase = TRUE, ...) {
      sql <- self$renderTranslateSql(query, ...)
      tryCatch({
        data <- self$queryFunction(sql, snakeCaseToCamelCase = snakeCaseToCamelCase)
      }, error = function(error) {
        if (self$connectionDetails$dbms %in% c("postgresql", "redshift")) {
          DatabaseConnector::dbExecute(self$con, "ABORT;")
        }
        stop(error)
      })

      return(dplyr::as_tibble(data))
    },
    queryFunction = function(sql, snakeCaseToCamelCase = TRUE) {
      DatabaseConnector::querySql(self$con, sql, snakeCaseToCamelCase = snakeCaseToCamelCase)
    }
  )
)

#' @importFrom pool dbPool poolClose
#' @importFrom DBI dbIsValid
PooledConnectionHandler <- R6::R6Class(
  "PooledConnectionHandler",
  inherit = ConnectionHandler,
  public = list(

    initConnection = function() {
      if (self$isActive) {
        warning("Closing existing connection")
        self$closeConnection()
      }

      self$con <- pool::dbPool(
        drv = DatabaseConnector::DatabaseConnectorDriver(),
        dbms = self$connectionDetails$dbms,
        server = self$connectionDetails$server(),
        port = self$connectionDetails$port(),
        user = self$connectionDetails$user(),
        password = self$connectionDetails$password()
      )
      self$isActive <- TRUE
    },

    closeConnection = function() {
      if (DBI::dbIsValid(dbObj = self$con)) {
        pool::poolClose(pool = self$con)
      }
      self$isActive <- FALSE
    },

    queryFunction = function(sql, snakeCaseToCamelCase = TRUE) {
      data <- DatabaseConnector::dbGetQuery(self$con, sql)
      if (snakeCaseToCamelCase) {
        colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
      } else {
        colnames(data) <- toupper(colnames(data))
      }
      return(data)
    }
  )
)
