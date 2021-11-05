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


#' CEM Web Backend Class
#' @description
#' An interface to the common evidence model that uses http requests
#' @field apiUrl url for the common evidence model hosted api instance
#' @import R6
#' @import checkmate
#' @export
CemWebApiBackend <- R6::R6Class(
  "CemWebApiBackend",
  inherit = AbstractCemBackend,
  public = list(
    apiUrl = NULL,

    #' @description
    #' initialize object
    #' @param apiUrl String URL parameter for hosted
    initialize = function(apiUrl) {
      # Remove trailing slash
      self$apiUrl <- gsub("/$", "", apiUrl)
      tryCatch(
        {
          self$getVersion()
        },
        error = function(err) {
          # Seems to happen when live app has been inactive for some time.
          Sys.sleep(0.5)
          self$getVersion()
        }
      )
    },

    #' @description
    #' Do a web request
    #' @param method string "POST", "GET", "PUT" (not implemented), "DELETE" (not implemented)
    #' @param endpoint URL endpoint string
    #' @param ... list params for httr method
    request = function(method, endpoint, ...) {
      url <- paste(self$apiUrl, endpoint, sep = "/")
      callFunc <- switch(method,
        "POST" = httr::POST,
        "GET" = httr::GET
      )

      response <- callFunc(url, encode = "json", ...)
      if (response$status_code != 200) {
        content <- httr::content(response, as = "parsed")
        stop("Request error ", content$error)
      }
      httr::content(response, as = "parsed")
    },

    #' @description
    #' GET server status
    getStatus = function() {
      self$request("GET", "")
    },

    #' @description
    #' Returns set of ingredient concepts for a given conceptset of outcomes
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getConditionEvidenceSummary = function(conditionConceptSet,
                                           siblingLookupLevels = 0) {
      endpoint <- "conditionEvidenceSummary"
      content <- self$request("POST", endpoint, body = list(
        conditionConceptSet = conditionConceptSet,
        siblingLookupLevels = siblingLookupLevels
      ))

      dplyr::bind_rows(content$result)
    },

    #' @description
    #' Returns set of outcome concepts for a given conceptset of ingredients
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    getIngredientEvidenceSummary = function(ingredientConceptSet) {
      endpoint <- "ingredientEvidenceSummary"
      content <- self$request("POST", endpoint, body = list(ingredientConceptSet = ingredientConceptSet))
      dplyr::bind_rows(content$result)
    },

    #' @description
    #' Returns ingredient evidence for a given conceptset of conditions
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getConditionEvidence = function(conditionConceptSet,
                                    siblingLookupLevels = 0) {
      endpoint <- "conditionEvidence"
      content <- self$request("POST", endpoint, body = list(
        conditionConceptSet = conditionConceptSet,
        siblingLookupLevels = siblingLookupLevels
      ))

      dplyr::bind_rows(content$result)
    },

    #' @description
    #' Returns condtion evidence for a given conceptset of ingredients
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    getIngredientEvidence = function(ingredientConceptSet) {
      endpoint <- "ingredientEvidence"
      content <- self$request("POST", endpoint, body = list(ingredientConceptSet = ingredientConceptSet))
      dplyr::bind_rows(content$result)
    },

    #' @description
    #' From a unified CEM relationships table, for a conceptSet of drug ingredients and a conceptSet of conditions,
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param conditionSiblingLookupLevels integer - where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getRelationships = function(ingredientConceptSet, conditionConceptSet, conditionSiblingLookupLevels = 0) {
      endpoint <- "relationships"
      content <- self$request("POST", endpoint, body = list(
        ingredientConceptSet = ingredientConceptSet,
        conditionConceptSet = conditionConceptSet,
        conditionSiblingLookupLevels = conditionSiblingLookupLevels
      ))
      dplyr::bind_rows(content$result)
    },

    #' @description
    #' Returns data.frame of sources that made the CEM
    getCemSourceInfo = function() {
      endpoint <- "cemSourceInfo"
      content <- self$request("GET", endpoint)
      dplyr::bind_rows(content$result)
    },

    #' @description
    #' getVersion information from api
    getVersion = function() {
      endpoint <- "version"
      self$request("GET", endpoint)
    },

    #' @description
    #' Get negative control snomed condition concepts for a given conceptset
    #' These are ranked by co-occurrence accross ohdsi studies
    #' A negative control for a submitted concept_set is valid if there is no evidence for the outcome
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    #' @param nControls topN controls to select - the maximum number will be limited by available concepts without related evidence
    #' @returns data.frame of condition concept_id and concept_name
    getSuggestedControlCondtions = function(ingredientConceptSet, nControls = 100) {
      endpoint <- "suggestedControlConditions"
      content <- self$request("POST", endpoint, body = list(
        ingredientConceptSet = ingredientConceptSet,
        nControls = nControls
      ))

      dplyr::bind_rows(content$result)
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
      endpoint <- "suggestedControlIngredients"
      content <- self$request("POST", endpoint, body = list(
        conditionConceptSet = conditionConceptSet,
        siblingLookupLevels = siblingLookupLevels,
        nControls = nControls
      ))

      dplyr::bind_rows(content$result)
    }
  )
)
