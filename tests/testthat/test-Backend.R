
# In future this will be loaded as either a database backend or a webservice/plumber client backend
# This will allow testing to see if the implementations return equivalent functioning responses
backend <- CEMDatabaseBackend$new(connectionDetails,
                                    cemSchema = cemTestSchema,
                                    vocabularySchema = vocabularySchema,
                                    sourceSchema = sourceInfoSchema)

withr::defer({
  backend$finalize()
}, testthat::teardown_env())


test_that("Backend loads", {
  expect_class(backend, "CEMDatabaseBackend")
  expect_class(backend$connection, "ConnectionHandler")
  expect_true(backend$connection$isActive)
  expect_equal(backend$cemSchema, cemTestSchema)
  expect_equal(backend$vocabularySchema, vocabularySchema)
  expect_equal(backend$sourceSchema, sourceInfoSchema)
})

test_that("summary works", {
  sinfo <- backend$getCemSourceInfo()
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
  expect_data_frame(ingredientConcepts)
  expect_true(nrow(ingredientConcepts) > 100)

  # Codene - common ingredient
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts)
  expect_true(nrow(outcomeConcepts) > 100)

  # ATC class test (Other opioids) - should not be in ingredients but should return set
  srchIngredientSet <- data.frame(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts)
  expect_true(nrow(outcomeConcepts) > 100)

  # Search grouped sets should not return repeat ids
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1,0), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts)
  expect_true(nrow(outcomeConcepts) > 100)
  expect_true(length(outcomeConcepts$conceptId) == length(unique(outcomeConcepts$conceptId)))
})

test_that("get exposure and outcome conceptset evidence", {
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1,0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)

  expect_data_frame(relationships)
  expect_true(nrow(relationships) > 0)
})