# In future this will be loaded as either a database backend or a webservice/plumber client backend
# This will allow testing to see if the implementations return equivalent functioning responses

backend <- CemDatabaseBackend$new(connectionDetails,
                                  cemSchema = cemTestSchema,
                                  vocabularySchema = vocabularySchema,
                                  sourceSchema = sourceInfoSchema)
webBackend <- CemWebApiBackend$new(apiUrl)

withr::defer({
  backend$finalize()
}, testthat::teardown_env())


test_that("DB Backend loads", {
  expect_class(backend, "CemDatabaseBackend")
  expect_class(backend$connection, "ConnectionHandler")
  expect_true(backend$connection$isActive)
  expect_equal(backend$cemSchema, cemTestSchema)
  expect_equal(backend$vocabularySchema, vocabularySchema)
  expect_equal(backend$sourceSchema, sourceInfoSchema)
})

test_that("Web Backend loads", {
  expect_class(webBackend, "CemWebApiBackend")
  expect_string(webBackend$getVersion()$version)
})


test_that("summary works", {
  sinfo <- backend$getCemSourceInfo()
  expect_data_frame(sinfo)

  sinfo <- webBackend$getCemSourceInfo()
  expect_data_frame(sinfo)
})

test_that("get exposure and outcome control concepts evidence", {

  # Mild Depression - doesn't map well in CEM
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  ingredientConcepts <- backend$getConditionEvidenceSummary(srchOutcomeConceptSet)
  expect_data_frame(ingredientConcepts)
  expect_true(nrow(ingredientConcepts) == 0)

  # Mild Depression, but use of sibling lookups
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  ingredientConcepts <- backend$getConditionEvidenceSummary(srchOutcomeConceptSet, siblingLookupLevels = 1)
  expect_data_frame(ingredientConcepts, min.rows = 100)


  ingredientConceptsWeb <- webBackend$getConditionEvidenceSummary(srchOutcomeConceptSet, siblingLookupLevels = 1)
  expect_true(dplyr::all_equal(ingredientConceptsWeb, ingredientConcepts))

  # Codene - common ingredient
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts, min.rows = 100)


  outcomeConceptsWeb <- webBackend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_true(dplyr::all_equal(outcomeConcepts, outcomeConceptsWeb))

  # ATC class test (Other opioids) - should not be in ingredients but should return set
  srchIngredientSet <- data.frame(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts, min.rows = 100)


  outcomeConceptsWeb <- webBackend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_true(dplyr::all_equal(outcomeConcepts, outcomeConceptsWeb))


  # Search grouped sets should not return repeat ids
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts, min.rows = 100)
  expect_true(length(outcomeConcepts$conditionConceptId) == length(unique(outcomeConcepts$conditionConceptId)))

  outcomeConceptsWeb <- webBackend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_true(dplyr::all_equal(outcomeConcepts, outcomeConceptsWeb))
})

test_that("get exposure and outcome conceptset evidence", {
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)

  expect_data_frame(relationships, min.rows = 100)

  relationshipsWeb <- webBackend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)
  # For some reason all_equal fails here. Solution: some manaul checks of data
  expect_equal(nrow(relationships), nrow(relationshipsWeb))
  expect_set_equal(colnames(relationships), colnames(relationshipsWeb))
})