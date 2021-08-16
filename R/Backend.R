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

#' CEM Abstract Backend Class
#' @description
#' Create subclass of this to use functionality.
#' #' @export
AbstractCemBackend <- R6::R6Class(
  "AbstractCemBackend",
  public = list(
    #' Initialize
    #' @description
    #' initialize backend object. Will fail in an error - this is an abstract class
    #' @param ... params
    initialize = function (...) {
      stop("Error: this is an abstract class. initialize function should be implemented by child")
    },

    #' @description
    #' Connection cleanup etc
    finalize = function() {}
  ),
  private = list(
    checkConceptSet = function(conceptSet) {
      checkmate::assert_data_frame(conceptSet, min.rows = 1)
      checkmate::checkNames(names(conceptSet), must.include = c("conceptId"))
    },

    getConceptIdsWithoutDescendants = function(conceptSet) {
      if (!("includeDescendants" %in% colnames(conceptSet))) {
        conceptSet$includeDescendants <- 1
      }

      if (!("isExcluded" %in% colnames(conceptSet))) {
        conceptSet$isExcluded <- 0
      }

      conceptSet[conceptSet$isExcluded == 0 & conceptSet$includeDescendants == 0,]$conceptId
    },

    getConceptIdsWithDescendants = function(conceptSet) {
      if (!("includeDescendants" %in% colnames(conceptSet))) {
        conceptSet$includeDescendants <- 1
      }

      if (!("isExcluded" %in% colnames(conceptSet))) {
        conceptSet$isExcluded <- 0
      }

      conceptSet[conceptSet$isExcluded == 0 & conceptSet$includeDescendants == 1,]$conceptId
    }
  )
)


#' CEM Database Backend Class
#' @description
#' An interface to the common evidence model that uses works directly with a database schema
#' @field connection ConnectionHandlder instance
#' @field cemSchema schema of CEM database
#' @field vocabularySchema OMOP vocabulary schema (must include concept and concept ancestor tables)
#' @field sourceSchema schema containing source_info table
#' @export
CemDatabaseBackend <- R6::R6Class(
  "CemDatabaseBackend",
  inherit = AbstractCemBackend,
  public = list(
    connection = NULL,
    cemSchema = NULL,
    vocabularySchema = NULL,
    sourceSchema = NULL,

    #' @description
    #' initialize backend object.
    #' @param connectionDetails DatabaseConnector connection details object
    #' @param cemSchema Schema name including CEM unified and matrix_summary tables
    #' @param vocabularySchema OMOP vocabulary
    #' @param sourceSchema Schema containing CEM source information table
    #' @param usePooledConnection Used a pooled connection object rather than a database connector object.
    initialize = function(connectionDetails,
                          cemSchema,
                          vocabularySchema,
                          sourceSchema,
                          usePooledConnection = FALSE) {
      checkmate::assert_class(connectionDetails, "connectionDetails")
      checkmate::assert_string(cemSchema, min.chars = 1)
      checkmate::assert_string(vocabularySchema, min.chars = 1)
      checkmate::assert_string(sourceSchema, min.chars = 1)

      if (usePooledConnection) {
        self$connection <- PooledConnectionHandler$new(connectionDetails)
      } else {
        self$connection <- ConnectionHandler$new(connectionDetails)
      }

      self$cemSchema <- cemSchema
      self$vocabularySchema <- vocabularySchema
      self$sourceSchema <- sourceSchema
    },

    #' @description
    #' Closes connection
    finalize = function() {
      self$connection$finalize()
    },

    #' Condition evidence summary
    #' @description
    #' Reutrns set of ingredient concepts for a given conceptset of outcomes
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getConditionEvidenceSummary = function(conditionConceptSet,
                                           siblingLookupLevels = 0) {

      private$checkConceptSet(conditionConceptSet)
      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)

      sql <- private$loadSqlFile("getConditionEvidenceSummary.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              use_siblings = siblingLookupLevels > 0,
                              sibling_lookup_levels = siblingLookupLevels,
                              condition_concept_desc = conditionConceptDesc,
                              condition_concept_no_desc = conditionConceptNoDesc)
    },

    #' Condition evidence
    #' @description
    #' Reutrns set of relationships that exist within CEM for a condition conceptset of interest
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getConditionEvidence = function(conditionConceptSet, siblingLookupLevels = 0) {

      private$checkConceptSet(conditionConceptSet)
      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)

      sql <- private$loadSqlFile("getConditionRelationships.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              use_siblings = siblingLookupLevels > 0,
                              sibling_lookup_levels = siblingLookupLevels,
                              condition_concept_desc = conditionConceptDesc,
                              condition_concept_no_desc = conditionConceptNoDesc) %>% dplyr::select(-id)
    },

    #' Ingredient evidence summary
    #' @description
    #' Reutrns set of outcome concepts for a given conceptset of ingredients/exposures
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    getIngredientEvidenceSummary = function(ingredientConceptSet) {
      private$checkConceptSet(ingredientConceptSet)
      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      sql <- private$loadSqlFile("getIngredientEvidenceSummary.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              concept_desc = ingredientConceptDesc,
                              concept_no_desc = ingredientConceptNoDesc)

    },

    #' Ingredient conceptset evidence
    #' @description
    #' for a concept set of rxnorm ingredients (or their ancestors) list the evidence stored in the CEM
    #' Utilises matrix summary table for optimisation.
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    getIngredientEvidence = function(ingredientConceptSet) {
      private$checkConceptSet(ingredientConceptSet)
      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      sql <- private$loadSqlFile("getIngredientRelationships.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              concept_desc = ingredientConceptDesc,
                              concept_no_desc = ingredientConceptNoDesc) %>% dplyr::select(-id)

    },

    #' @description
    #' From a unified CEM relationships table, for a conceptSet of drug ingredients and a conceptSet of conditions,
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param conditionSiblingLookupLevels integer - where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getRelationships = function(ingredientConceptSet, conditionConceptSet, conditionSiblingLookupLevels = 0) {
      private$checkConceptSet(ingredientConceptSet)
      private$checkConceptSet(conditionConceptSet)

      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)


      sql <- private$loadSqlFile("getRelationships.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              use_siblings = conditionSiblingLookupLevels > 0,
                              sibling_lookup_levels = conditionSiblingLookupLevels,
                              ingredient_concepts_desc = ingredientConceptDesc,
                              ingredient_concepts_no_desc = ingredientConceptNoDesc,
                              condition_concepts_desc = conditionConceptDesc,
                              condition_concepts_no_desc = conditionConceptNoDesc) %>% dplyr::select(-id)
    },

    #' @description
    #' Get CEM source info as a dataframe
    #' @returns data.frame of sources
    getCemSourceInfo = function() {
      return(self$connection$queryDb("SELECT {@limit_row_count != ''} ? {TOP @limit_row_count} * FROM (SELECT * FROM @schema.source) results;", 
				     limit_row_count = Sys.getenv("LIMIT_ROW_COUNT"),
				     schema = self$sourceSchema))
    },

    #' @description
    #' Get negative control snomed condition concepts for a given conceptset
    #' These are ranked by co-occurence accross ohdsi studies
    #' A negative control for a submitted concept_set is valid if there is no evidence for the outcome
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    #' @param nControls topN controls to select - the maximum number will be limited by available concepts without related evidence
    #' @returns data.frame of condition concept_id and concept_name
    getSuggestedControlCondtions = function(ingredientConceptSet, nControls = 50) {
      private$checkConceptSet(ingredientConceptSet)
      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      sql <- private$loadSqlFile("getRankedNcOutcomes.sql")
      self$connection$queryDb(sql,
                              n_controls = nControls,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              concept_desc = ingredientConceptDesc,
                              concept_no_desc = ingredientConceptNoDesc)


    },

    #' @description
    #' Get negative control rxnorm ingredient concepts for a given conceptset
    #' These are ranked by co-occurence accross ohdsi studies
    #' A negative control for a submitted concept_set is valid if there is no evidence for the ingredient/condition combination
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    #' @param nControls topN controls to select - the maximum number will be limited by available concepts without related evidence
    #' @returns data.frame of condition concept_id and concept_name
    getSuggestedControlIngredients = function(conditionConceptSet, siblingLookupLevels = 0, nControls = 50) {
      private$checkConceptSet(conditionConceptSet)
      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)

      sql <- private$loadSqlFile("getRankedNcExposures.sql")
      self$connection$queryDb(sql,
                              n_controls = nControls,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              use_siblings = siblingLookupLevels > 0,
                              sibling_lookup_levels = siblingLookupLevels,
                              condition_concept_desc = conditionConceptDesc,
                              condition_concept_no_desc = conditionConceptNoDesc)
    }
  ),

  private = list(
    loadSqlFile = function(sqlFilename) {
      pathToSql <- system.file(paste("sql/sql_server"),
                               sqlFilename,
                               package = "CemConnector",
                               mustWork = TRUE)
      sql <- SqlRender::readSql(pathToSql)
    }
  )
)

