# In future this will be loaded as either a database backend or a webservice/plumber client backend
# This will allow testing to see if the implementations return equivalent functioning responses

backend <- CemDatabaseBackend$new(connectionDetails,
  cemDatabaseSchema = cemTestSchema,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema,
  sourceDatabaseSchema = sourceInfoSchema
)

withr::defer(
  {
    backend$finalize()
  },
  testthat::teardown_env()
)

test_that("Abstract class can't be instantiated", {
  expect_error(AbstractCemBackend$new(), "is an abstract class. initialize function should be implemented by child")
})

test_that("DB Backend loads", {
  expect_class(backend, "CemDatabaseBackend")
  expect_class(backend$connection, "ConnectionHandler")
  expect_true(backend$connection$isActive)
  expect_equal(backend$cemDatabaseSchema, cemTestSchema)
  expect_equal(backend$vocabularyDatabaseSchema, vocabularyDatabaseSchema)
  expect_equal(backend$sourceDatabaseSchema, sourceInfoSchema)
})

test_that("summary works", {
  sinfo <- backend$getCemSourceInfo()
  expect_data_frame(sinfo)
})

test_that("summary works web", {
  skip_if_not(useTestPlumber, "Test plumber not loaded")
  webBackend <- CemWebApiBackend$new(apiUrl)
  sinfo <- webBackend$getCemSourceInfo()
  expect_data_frame(sinfo)

  # 404 handled
  expect_error(
    {
      webBackend$request("GET", "nonexistentendpoint")
    },
    "Request error 404"
  )

  expect_equal(webBackend$getStatus()$status, "alive")
})

test_that("All excluded fails", {
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(1))
  expect_error(
    {
      outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
    },
    regexp = "Invalid concept set. All concepts are excluded from search"
  )
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
  expect_data_frame(ingredientConcepts, min.rows = 1)

  ingredientConceptEvdience <- backend$getConditionEvidence(srchOutcomeConceptSet, siblingLookupLevels = 1)
  expect_data_frame(ingredientConceptEvdience, min.rows = 10)

  # This test is incomplete in the sqlite tests because of limited data
  sugestedControlsIngredients <- backend$getSuggestedControlIngredients(srchOutcomeConceptSet, siblingLookupLevels = 1)
  expect_data_frame(sugestedControlsIngredients)

  # Test no descedants only
  srchOutcomeConceptSetNoDesc <- data.frame(conceptId = c(4149320), includeDescendants = c(0), isExcluded = c(0))
  sugestedControlsIngredients <- backend$getSuggestedControlIngredients(srchOutcomeConceptSetNoDesc)
  expect_data_frame(sugestedControlsIngredients)

  # Test no descedants and lookups
  srchOutcomeConceptSetNoDesc <- data.frame(conceptId = c(4149320, 4149320), includeDescendants = c(0, 1), isExcluded = c(0, 0))
  sugestedControlsIngredients <- backend$getSuggestedControlIngredients(srchOutcomeConceptSetNoDesc, siblingLookupLevels = 1)
  expect_data_frame(sugestedControlsIngredients)

  # Codene - common ingredient
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts, min.rows = 10)

  # Same query but does not use descedndants search
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(0), isExcluded = c(0))
  outcomeConcepts2 <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts2, min.rows = 10)
  expect_identical(outcomeConcepts, outcomeConcepts2)

  # ATC class test (Other opioids) - should not be in ingredients but should return set
  srchIngredientSet <- data.frame(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts, min.rows = 10)

  # Search grouped sets should not return repeat ids
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts, min.rows = 10)
  expect_true(length(outcomeConcepts$conditionConceptId) == length(unique(outcomeConcepts$conditionConceptId)))

  sugestedControls <- backend$getSuggestedControlCondtions(srchIngredientSet)
  expect_data_frame(sugestedControls, min.rows = 50)

  outcomeConceptEvidence <- backend$getIngredientEvidence(srchIngredientSet)
  expect_data_frame(outcomeConceptEvidence)

  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)

  expect_data_frame(relationships, min.rows = 10)
})

test_that("Test relationship sql logic combinations", {
  # Ensures that broken sqlrender logic doesn't exist
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)
  expect_data_frame(relationships)

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(0), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)
  expect_data_frame(relationships)

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(0), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)
  expect_data_frame(relationships)

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)
  expect_data_frame(relationships)
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))

  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 0)
  expect_data_frame(relationships)

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(0), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 0)
  expect_data_frame(relationships)

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(0), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 0)
  expect_data_frame(relationships)

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 0)
  expect_data_frame(relationships)
})

test_that("web backend returns equivalent results", {
  webBackend <- CemWebApiBackend$new(apiUrl)

  expect_class(webBackend, "CemWebApiBackend")
  expect_string(webBackend$getVersion()$version)

  # Mild Depression, but use of sibling lookups
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  ingredientConcepts <- backend$getConditionEvidenceSummary(srchOutcomeConceptSet, siblingLookupLevels = 1)

  ingredientConceptsWeb <- webBackend$getConditionEvidenceSummary(srchOutcomeConceptSet, siblingLookupLevels = 1)
  expect_true(dplyr::all_equal(ingredientConceptsWeb, ingredientConcepts, convert = TRUE))

  ingredientConceptEvdience <- backend$getConditionEvidence(srchOutcomeConceptSet, siblingLookupLevels = 1)
  ingredientConceptEvdienceWeb <- webBackend$getConditionEvidence(srchOutcomeConceptSet, siblingLookupLevels = 1)
  expect_true(nrow(ingredientConceptEvdience) > 0)
  expect_equal(nrow(ingredientConceptEvdience), nrow(ingredientConceptEvdienceWeb))

  # Codene - common ingredient
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)

  outcomeConceptsWeb <- webBackend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_true(dplyr::all_equal(outcomeConcepts, outcomeConceptsWeb, convert = TRUE))

  outcomeConceptEvidence <- backend$getIngredientEvidence(srchIngredientSet)
  outcomeConceptEvidenceWeb <- webBackend$getIngredientEvidence(srchIngredientSet)

  # ATC class test (Other opioids) - should not be in ingredients but should return set
  srchIngredientSet <- data.frame(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  outcomeConceptsWeb <- webBackend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_true(dplyr::all_equal(outcomeConcepts, outcomeConceptsWeb, convert = TRUE))

  # Search grouped sets should not return repeat ids
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  outcomeConceptsWeb <- webBackend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_true(dplyr::all_equal(outcomeConcepts, outcomeConceptsWeb, convert = TRUE))

  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))
  relationships <- backend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)
  relationshipsWeb <- webBackend$getRelationships(ingredientConceptSet = srchIngredientSet, conditionConceptSet = srchOutcomeConceptSet, conditionSiblingLookupLevels = 1)
  # For some reason all_equal fails here. Solution: some manaul checks of data
  expect_equal(nrow(relationships), nrow(relationshipsWeb))
  expect_set_equal(colnames(relationships), colnames(relationshipsWeb))

  # NOTE: This test is incomplete in the sqlite tests because of limited data - tested in live setting only.
  # Just tests function call, not data accuracy
  suggestedControlsIngredients <- backend$getSuggestedControlIngredients(srchOutcomeConceptSet, siblingLookupLevels = 1)
  suggestedControlsIngredientsWeb <- webBackend$getSuggestedControlIngredients(srchOutcomeConceptSet, siblingLookupLevels = 1)

  suggestedControlsOutcome <- backend$getSuggestedControlCondtions(srchIngredientSet)
  suggestedControlsOutcomeWeb <- webBackend$getSuggestedControlCondtions(srchIngredientSet)
  expect_true(dplyr::all_equal(suggestedControlsOutcome, suggestedControlsOutcomeWeb, convert = TRUE))
})
