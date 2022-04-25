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
#' @import checkmate
#' @import R6
AbstractCemBackend <- R6::R6Class(
  "AbstractCemBackend",
  public = list(
    #' @description
    #' initialize backend object. Will fail in an error - this is an abstract class
    #' @param ... params
    initialize = function(...) {
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
      if (!("includeDescendants" %in% colnames(conceptSet))) {
        conceptSet$includeDescendants <- 1
      }

      if (!("isExcluded" %in% colnames(conceptSet))) {
        conceptSet$isExcluded <- 0
      }
      # Cannot allow concept sets of all 0
      if (all(as.logical(conceptSet$isExcluded))) {
        stop("Invalid concept set. All concepts are excluded from search")
      }
      return(conceptSet)
    },
    getConceptIdsWithoutDescendants = function(conceptSet) {
      conceptSet[conceptSet$isExcluded == 0 & conceptSet$includeDescendants == 0, ]$conceptId
    },
    getConceptIdsWithDescendants = function(conceptSet) {
      conceptSet[conceptSet$isExcluded == 0 & conceptSet$includeDescendants == 1, ]$conceptId
    },
    getVersion = function() {
      paste(utils::packageVersion("CemConnector"))
    }
  )
)


#' CEM Database Backend Class
#' @description
#' An interface to the common evidence model that uses works directly with a database schema
#' @field connection ConnectionHandlder instance
#' @field cemDatabaseSchema schema of CEM database
#' @field vocabularyDatabaseSchema OMOP vocabulary schema (must include concept and concept ancestor tables)
#' @field sourceDatabaseSchema schema containing source_info table
#' @import checkmate
#' @import R6
#' @export
CemDatabaseBackend <- R6::R6Class(
  "CemDatabaseBackend",
  inherit = AbstractCemBackend,
  public = list(
    connection = NULL,
    cemDatabaseSchema = NULL,
    vocabularyDatabaseSchema = NULL,
    sourceDatabaseSchema = NULL,

    #' @description
    #' initialize backend object.
    #' @param connectionDetails DatabaseConnector connection details object
    #' @param cemDatabaseSchema Schema name including CEM unified and matrix_summary tables
    #' @param vocabularyDatabaseSchema OMOP vocabulary
    #' @param sourceDatabaseSchema Schema containing CEM source information table
    #' @param usePooledConnection Used a pooled connection object rather than a database connector object.
    initialize = function(connectionDetails,
                          cemDatabaseSchema,
                          vocabularyDatabaseSchema,
                          sourceDatabaseSchema,
                          usePooledConnection = FALSE) {
      checkmate::assert_class(connectionDetails, "connectionDetails")
      checkmate::assert_string(cemDatabaseSchema, min.chars = 1)
      checkmate::assert_string(vocabularyDatabaseSchema, min.chars = 1)
      checkmate::assert_string(sourceDatabaseSchema, min.chars = 1)

      if (usePooledConnection) {
        self$connection <- PooledConnectionHandler$new(connectionDetails)
      } else {
        self$connection <- ConnectionHandler$new(connectionDetails)
      }

      self$cemDatabaseSchema <- cemDatabaseSchema
      self$vocabularyDatabaseSchema <- vocabularyDatabaseSchema
      self$sourceDatabaseSchema <- sourceDatabaseSchema
    },

    #' @description
    #' Closes connection
    finalize = function() {
      self$connection$finalize()
    },

    #' Condition evidence summary
    #' @description
    #' Returns set of ingredient concepts for a given conceptset of outcomes
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getConditionEvidenceSummary = function(conditionConceptSet,
                                           siblingLookupLevels = 0) {
      conditionConceptSet <- private$checkConceptSet(conditionConceptSet)
      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)

      sql <- private$loadSqlFile("getConditionEvidenceSummary.sql")
      self$connection$queryDb(sql,
        vocabulary = self$vocabularyDatabaseSchema,
        cem_schema = self$cemDatabaseSchema,
        use_siblings = siblingLookupLevels > 0,
        sibling_lookup_levels = siblingLookupLevels,
        condition_concept_desc = conditionConceptDesc,
        condition_concept_no_desc = conditionConceptNoDesc
      )
    },

    #' Condition evidence
    #' @description
    #' Returns set of relationships that exist within CEM for a condition conceptset of interest
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getConditionEvidence = function(conditionConceptSet, siblingLookupLevels = 0) {
      conditionConceptSet <- private$checkConceptSet(conditionConceptSet)
      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)

      sql <- private$loadSqlFile("getConditionRelationships.sql")
      self$connection$queryDb(sql,
        vocabulary = self$vocabularyDatabaseSchema,
        cem_schema = self$cemDatabaseSchema,
        use_siblings = siblingLookupLevels > 0,
        sibling_lookup_levels = siblingLookupLevels,
        condition_concept_desc = conditionConceptDesc,
        condition_concept_no_desc = conditionConceptNoDesc
      ) %>% dplyr::select(-id)
    },

    #' Ingredient evidence summary
    #' @description
    #' Returns set of outcome concepts for a given conceptset of ingredients/exposures
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    getIngredientEvidenceSummary = function(ingredientConceptSet) {
      ingredientConceptSet <- private$checkConceptSet(ingredientConceptSet)
      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      sql <- private$loadSqlFile("getIngredientEvidenceSummary.sql")
      self$connection$queryDb(sql,
        vocabulary = self$vocabularyDatabaseSchema,
        cem_schema = self$cemDatabaseSchema,
        concept_desc = ingredientConceptDesc,
        concept_no_desc = ingredientConceptNoDesc
      )
    },

    #' Ingredient conceptset evidence
    #' @description
    #' for a concept set of rxnorm ingredients (or their ancestors) list the evidence stored in the CEM
    #' Utilises matrix summary table for optimisation.
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    getIngredientEvidence = function(ingredientConceptSet) {
      ingredientConceptSet <- private$checkConceptSet(ingredientConceptSet)
      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      sql <- private$loadSqlFile("getIngredientRelationships.sql")
      self$connection$queryDb(sql,
        vocabulary = self$vocabularyDatabaseSchema,
        cem_schema = self$cemDatabaseSchema,
        concept_desc = ingredientConceptDesc,
        concept_no_desc = ingredientConceptNoDesc
      ) %>% dplyr::select(-id)
    },

    #' @description
    #' From a unified CEM relationships table, for a conceptSet of drug ingredients and a conceptSet of conditions,
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param conditionSiblingLookupLevels integer - where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getRelationships = function(ingredientConceptSet, conditionConceptSet, conditionSiblingLookupLevels = 0) {
      ingredientConceptSet <- private$checkConceptSet(ingredientConceptSet)
      conditionConceptSet <- private$checkConceptSet(conditionConceptSet)
      conditionConceptSet <- private$checkConceptSet(conditionConceptSet)

      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)


      sql <- private$loadSqlFile("getRelationships.sql")
      self$connection$queryDb(sql,
        vocabulary = self$vocabularyDatabaseSchema,
        cem_schema = self$cemDatabaseSchema,
        use_siblings = conditionSiblingLookupLevels > 0,
        sibling_lookup_levels = conditionSiblingLookupLevels,
        ingredient_concepts_desc = ingredientConceptDesc,
        ingredient_concepts_no_desc = ingredientConceptNoDesc,
        condition_concepts_desc = conditionConceptDesc,
        condition_concepts_no_desc = conditionConceptNoDesc
      ) %>% dplyr::select(-id)
    },

    #' @description
    #' Get CEM source info as a dataframe
    #' @returns data.frame of sources
    getCemSourceInfo = function() {
      return(self$connection$queryDb("SELECT * FROM @schema.source;", schema = self$sourceDatabaseSchema))
    },

    #' @description
    #' Get negative control snomed condition concepts for a given conceptset
    #' These are ranked by co-occurrence accross ohdsi studies
    #' A negative control for a submitted concept_set is valid if there is no evidence for the outcome
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    #' @param nControls topN controls to select - the maximum number will be limited by available concepts without related evidence
    #' @returns data.frame of condition concept_id and concept_name
    getSuggestedControlCondtions = function(ingredientConceptSet, nControls = 100) {
      ingredientConceptSet <- private$checkConceptSet(ingredientConceptSet)
      ingredientConceptNoDesc <- private$getConceptIdsWithoutDescendants(ingredientConceptSet)
      ingredientConceptDesc <- private$getConceptIdsWithDescendants(ingredientConceptSet)

      sql <- private$loadSqlFile("getRankedNcOutcomes.sql")
      self$connection$queryDb(sql,
        n_controls = nControls,
        vocabulary = self$vocabularyDatabaseSchema,
        cem_schema = self$cemDatabaseSchema,
        concept_desc = ingredientConceptDesc,
        concept_no_desc = ingredientConceptNoDesc
      )
    },

    #' @description
    #' Get negative control rxnorm ingredient concepts for a given conceptset
    #' These are ranked by co-occurrence accross ohdsi studies
    #' A negative control for a submitted concept_set is valid if there is no evidence for the ingredient/condition combination
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    #' @param nControls topN controls to select - the maximum number will be limited by available concepts without related evidence
    #' @returns data.frame of condition concept_id and concept_name
    getSuggestedControlIngredients = function(conditionConceptSet, siblingLookupLevels = 0, nControls = 100) {
      conditionConceptSet <- private$checkConceptSet(conditionConceptSet)
      conditionConceptDesc <- private$getConceptIdsWithDescendants(conditionConceptSet)
      conditionConceptNoDesc <- private$getConceptIdsWithoutDescendants(conditionConceptSet)

      sql <- private$loadSqlFile("getRankedNcExposures.sql")
      self$connection$queryDb(sql,
        n_controls = nControls,
        vocabulary = self$vocabularyDatabaseSchema,
        cem_schema = self$cemDatabaseSchema,
        use_siblings = siblingLookupLevels > 0,
        sibling_lookup_levels = siblingLookupLevels,
        condition_concept_desc = conditionConceptDesc,
        condition_concept_no_desc = conditionConceptNoDesc
      )
    }
  ),
  private = list(
    loadSqlFile = function(sqlFilename) {
      pathToSql <- system.file(paste("sql/sql_server"),
        sqlFilename,
        package = "CemConnector",
        mustWork = TRUE
      )
      sql <- SqlRender::readSql(pathToSql)
    }
  )
)

