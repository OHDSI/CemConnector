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

.loadSqlFile <- function(sqlFilename) {
  pathToSql <- system.file(paste("sql/sql_server"),
                           sqlFilename,
                           package = "CEMConnector",
                           mustWork = TRUE)
  sql <- SqlRender::readSql(pathToSql)
}

#' @export
CEMDatabaseBackend <- R6::R6Class(
  "CEMDatabaseBackend",
  public = list(
    connection = NULL,
    cemSchema = NULL,
    vocabularySchema = NULL,
    sourceSchema = NULL,
    initialize = function(connectionDetails,
                          cemSchema,
                          vocabularySchema,
                          sourceSchema,
                          usePooledConnection = FALSE) {
      if (usePooledConnection) {
        self$connection <- PooledConnectionHandler$new(connectionDetails)
      } else {
        self$connection <- ConnectionHandler$new(connectionDetails)
      }

      self$cemSchema <- cemSchema
      self$vocabularySchema <- vocabularySchema
      self$sourceSchema <- sourceSchema
    },

    finalize = function() {
      self$connection$finalize()
    },

      #' @description
      #' Reutrns set of ingredient concepts for a given conceptset of outcomes
      #'
    getConditionEvidenceSummary = function(conditionConceptSet,
                                           siblingLookupLevels = 0) {

      checkmate::assert_data_frame(conditionConceptSet)
      checkmate::checkNames(names(conditionConceptSet), must.include = c("includeDescendants", "conceptId", "isExcluded"))
      conditionConceptDesc <- conditionConceptSet[conditionConceptSet$isExcluded == 0 & conditionConceptSet$includeDescendants == 1,]$conceptId
      conditionConceptNoDesc <- conditionConceptSet[conditionConceptSet$isExcluded == 0 & conditionConceptSet$includeDescendants == 0,]$conceptId
      sql <- .loadSqlFile("getConditionEvidenceSummary.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              use_siblings = siblingLookupLevels > 0,
                              sibling_lookup_levels = siblingLookupLevels,
                              condition_concept_desc = conditionConceptDesc,
                              condition_concept_no_desc = conditionConceptNoDesc)
    },

      #' @description
      #' Reutrns set of outcome concepts for a given conceptset of ingredients/exposures
      #'
    getIngredientEvidenceSummary = function(ingredientConceptSet) {
      checkmate::assert_data_frame(ingredientConceptSet)
      checkmate::checkNames(names(ingredientConceptSet), must.include = c("includeDescendants", "conceptId", "isExcluded"))
      ingredientConceptDesc <- ingredientConceptSet[ingredientConceptSet$isExcluded == 0 & ingredientConceptSet$includeDescendants == 1,]$conceptId
      ingredientConceptNoDesc <- ingredientConceptSet[ingredientConceptSet$isExcluded == 0 & ingredientConceptSet$includeDescendants == 0,]$conceptId

      sql <- .loadSqlFile("getIngredientEvidenceSummary.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              concept_desc = ingredientConceptDesc,
                              concept_no_desc = ingredientConceptNoDesc)

    },


    #' @description
    #' From a unified CEM relationships table, for a conceptSet of drug ingredients and a conceptSet of conditions,
    #' return all the related evidence across all sources
    getRelationships = function(ingredientConceptSet, conditionConceptSet, conditionSiblingLookupLevels = 0) {
      checkmate::assert_data_frame(ingredientConceptSet)
      checkmate::checkNames(names(ingredientConceptSet), must.include = c("includeDescendants", "conceptId", "isExcluded"))
      ingredientConceptDesc <- ingredientConceptSet[ingredientConceptSet$isExcluded == 0 & ingredientConceptSet$includeDescendants == 1,]$conceptId
      ingredientConceptNoDesc <- ingredientConceptSet[ingredientConceptSet$isExcluded == 0 & ingredientConceptSet$includeDescendants == 0,]$conceptId

      checkmate::assert_data_frame(conditionConceptSet)
      checkmate::checkNames(names(conditionConceptSet), must.include = c("includeDescendants", "conceptId", "isExcluded"))
      conditionConceptDesc <- conditionConceptSet[conditionConceptSet$isExcluded == 0 & conditionConceptSet$includeDescendants == 1,]$conceptId
      conditionConceptNoDesc <- conditionConceptSet[conditionConceptSet$isExcluded == 0 & conditionConceptSet$includeDescendants == 0,]$conceptId


      sql <- .loadSqlFile("getRelationships.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              use_siblings = conditionSiblingLookupLevels > 0,
                              sibling_lookup_levels = conditionSiblingLookupLevels,
                              ingredient_concepts_desc = ingredientConceptDesc,
                              ingredient_concepts_no_desc = ingredientConceptNoDesc,
                              condition_concepts_desc = conditionConceptDesc,
                              condition_concepts_no_desc = conditionConceptNoDesc)
    },

    getCemSourceInfo = function() {
      return(self$connection$queryDb("SELECT * FROM @schema.source", schema = self$sourceSchema))
    }
  )
)
