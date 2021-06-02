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

CEMWebApiBackend <- R6::R6Class(
  "CEMWebApiBackend",
  public = list(
    apiUrl = NULL,

    initialize = function(apiUrl, testValidUrl = TRUE) {
      self$apiUrl <- apiUrl
      assert(self$getStatus()$status == "alive")
    },

    request = function(method, endpoint, ...) {
      url <- paste(self$apiUrl, endpoint, sep = "/")
      callFunc <- switch(method,
                         "POST" = httr::POST,
                         "GET" = httr::GET)

      response <- callFunc(url, encode = "json", ...)
      httr::content(response, as = "parsed")
    },

    getStatus = function (){
      self$request("GET", "")
    },

    #' @description
    #' Reutrns set of ingredient concepts for a given conceptset of outcomes
    getConditionEvidenceSummary = function(conditionConceptSet,
                                           siblingLookupLevels = 0) {
      endpoint <- "conditionEvidenceSummary"
      content <- self$request("POST", endpoint, body = list(conditionConceptSet = conditionConceptSet,
                                                            siblingLookupLevels = siblingLookupLevels))

      dplyr::bind_rows(content$result)
    },

    #' @description
    #' Reutrns set of outcome concepts for a given conceptset of ingredients/exposures
    getIngredientEvidenceSummary = function(ingredientConceptSet) {
      endpoint <- "ingredientEvidenceSummary"
      content <- self$request("POST", endpoint, body = list(ingredientConceptSet = ingredientConceptSet))
      dplyr::bind_rows(content$result)
    },

    #' @description
    #' From a unified CEM relationships table, for a conceptSet of drug ingredients and a conceptSet of conditions,
    #' return all the related evidence across all sources
    getRelationships = function(ingredientConceptSet, conditionConceptSet, conditionSiblingLookupLevels = 0) {
      endpoint <- "relationships"
      content <- self$request("POST", endpoint, body = list(ingredientConceptSet = ingredientConceptSet,
                                                            conditionConceptSet = conditionConceptSet,
                                                            conditionSiblingLookupLevels = conditionSiblingLookupLevels))
      dplyr::bind_rows(content$result)
    },

    getCemSourceInfo = function() {
      endpoint <- "cemSourceInfo"
      content <- self$request("GET", endpoint)
      dplyr::bind_rows(content$result)
    },

    getVersion = function() {
      endpoint <- "version"
      self$request("GET", endpoint)
    }
  )
)