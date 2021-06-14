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
#' @export
CemWebApiBackend <- R6::R6Class(
  "CEMWebApiBackend",
  public = list(
    apiUrl = NULL,

     #' @description
     #' initialzie object
     #' @param apiUrl String URL parameter for hosted
    initialize = function(apiUrl) {
      self$apiUrl <- apiUrl
      assert(self$getStatus()$status == "alive")
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
                         "GET" = httr::GET)

      response <- callFunc(url, encode = "json", ...)
      httr::content(response, as = "parsed")
    },

    #' @description
    #' GET server status
    getStatus = function (){
      self$request("GET", "")
    },

    #' @description
    #' Reutrns set of ingredient concepts for a given conceptset of outcomes
    #' @param conditionConceptSet data.frame conforming to conceptset format, must be standard SNOMED conditions
    #' @param siblingLookupLevels where mapping is not found it may be beneficial to lookup siblings in the concept ancestry. This defines the number of levels to jump
    getConditionEvidenceSummary = function(conditionConceptSet,
                                           siblingLookupLevels = 0) {
      endpoint <- "conditionEvidenceSummary"
      content <- self$request("POST", endpoint, body = list(conditionConceptSet = conditionConceptSet,
                                                            siblingLookupLevels = siblingLookupLevels))

      dplyr::bind_rows(content$result)
    },

    #' @description
    #' Reutrns set of outcome concepts for a given conceptset of ingredients/exposures
    #' @param ingredientConceptSet data.frame conforming to conceptset format, must be standard RxNorm Ingredients
    getIngredientEvidenceSummary = function(ingredientConceptSet) {
      endpoint <- "ingredientEvidenceSummary"
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
      content <- self$request("POST", endpoint, body = list(ingredientConceptSet = ingredientConceptSet,
                                                            conditionConceptSet = conditionConceptSet,
                                                            conditionSiblingLookupLevels = conditionSiblingLookupLevels))
      dplyr::bind_rows(content$result)
    },

    #' @description
    #' Returns datframe of sources that made the CEM
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
    }
  )
)