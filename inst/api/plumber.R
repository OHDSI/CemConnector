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

# The API must be loaded with CEMBackendApi variable that has a predicted interface
# This class largely exposes database connections and passes them to a JSON serializer
checkmate::assert_class(cemBackendApi, "CemDatabaseBackend")

#* Get API status
#* @get /
#* @serializer unboxedJSON
function() {
  list(status = 'alive')
}

#* Get CemConnector API version info
#* @get /version
#* @serializer unboxedJSON
function() {
  list(version = paste0(packageVersion("CemConnector")))
}

#* Get CemConnector API version info
#* @get /cemSourceInfo
#* @serializer unboxedJSON
function() {
  list(result = cemBackendApi$getCemSourceInfo())
}

#* Get For a set of standard condition concepts get the evidence summary
#* @param conditionConceptSet conceptset of conditions. Must be in conceptSet format and use only OMOP standard concepts
#* @param siblingLookupLevels number of sibling levels to look up to find matches
#* @post /conditionEvidenceSummary
#* @serializer unboxedJSON
function(req) {
  conditionConceptSet <- req$body$conditionConceptSet
  conceptSetDataFrame <- rbind.data.frame(conditionConceptSet)

  siblingLookupLevels <- req$body$siblingLookupLevels
  if (is.null(siblingLookupLevels)) {
    siblingLookupLevels <- 0
  }
  result <- cemBackendApi$getConditionEvidenceSummary(conceptSetDataFrame, siblingLookupLevels = siblingLookupLevels)

  list(result = result)
}

#* Get For a set of OMOP standard vocabulary ingredient concepts get the evidence summary of conditions
#* This returns the set of sumarized evidence for every condition in the CEM
#* @param ingredientConceptSet conceptset of drug ingredients. Must be in conceptSet format and use only OMOP standard concepts
#* @post /ingredientEvidenceSummary
#* @serializer unboxedJSON
function(req) {
  ingredientConceptSet <- req$body$ingredientConceptSet
  ingredientConceptSetDf <- rbind.data.frame(ingredientConceptSet)
  result <- cemBackendApi$getIngredientEvidenceSummary(ingredientConceptSetDf)

  list(result = result)
}

#* Get For a set of OMOP standard vocabulary ingredient and drug concepts get the evidence summary of conditions
#* This returns the set of sumarized evidence for every condition in the CEM
#* @param conditionConceptSet conceptset of conditions. Must be in conceptSet format and use only OMOP standard concepts
#* @param ingredientConceptSet conceptset of drug ingredients. Must be in conceptSet format and use only OMOP standard concepts
#* @post /relationships
#* @serializer unboxedJSON
function(req) {
  conditionConceptSet <- req$body$conditionConceptSet
  conditionConceptSetDf <- rbind.data.frame(conditionConceptSet)

  conditionSiblingLookupLevels <- req$body$conditionSiblingLookupLevels
  if (is.null(conditionSiblingLookupLevels)) {
    conditionSiblingLookupLevels <- 0
  }

  ingredientConceptSet <- req$body$ingredientConceptSet
  ingredientConceptSetDf <- rbind.data.frame(ingredientConceptSet)

  result <- cemBackendApi$getRelationships(ingredientConceptSetDf,
                                           conditionConceptSetDf,
                                           conditionSiblingLookupLevels = conditionSiblingLookupLevels)

  list(result = result)
}


#* Get For a set of OMOP standard vocabulary ingredient and drug concepts get the evidence summary of conditions
#* This returns the set of sumarized evidence for every condition in the CEM
#* @param ingredientConceptSet conceptset of drug ingredients. Must be in conceptSet format and use only OMOP standard concepts
#* @post /ingredientRelationships
#* @serializer unboxedJSON
function(req) {
  ingredientConceptSet <- req$body$ingredientConceptSet
  ingredientConceptSetDf <- rbind.data.frame(ingredientConceptSet)

  result <- cemBackendApi$getIngredientRelationships(ingredientConceptSetDf)

  list(result = result)
}


#* Get For a set of OMOP standard vocabulary ingredient and drug concepts get the evidence summary of conditions
#* This returns the set of sumarized evidence for every condition in the CEM
#* @param conditionConceptSet conceptset of conditions. Must be in conceptSet format and use only OMOP standard concepts
#* @post /conditionRelationships
#* @serializer unboxedJSON
function(req) {
  conditionConceptSet <- req$body$conditionConceptSet
  conditionConceptSetDf <- rbind.data.frame(conditionConceptSet)
  conditionSiblingLookupLevels <- req$body$conditionSiblingLookupLevels
  if (is.null(conditionSiblingLookupLevels)) {
    conditionSiblingLookupLevels <- 0
  }

  result <- cemBackendApi$getConditionRelationships(conditionConceptSetDf,
                                                    conditionSiblingLookupLevels = conditionSiblingLookupLevels)

  list(result = result)
}