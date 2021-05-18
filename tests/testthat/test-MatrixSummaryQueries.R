connectionDetails <- DatabaseConnector::createConnectionDetails(server = "ohda-prod-1.cldcoxyrkflo.us-east-1.redshift.amazonaws.com/cem",
                                                                user = "jgilber2",
                                                                password = keyring::key_get("ohda-prod-1", username = "jgilber2"),
                                                                port = 5439,
                                                                dbms = "redshift",
                                                                extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory")

backend <- DBBackend$new(connectionDetails,
                         cemSchema = "cem_v3_0_0_20210308_evidence",
                         vocabularySchema = "cem_v3_0_0_20210308_staging_vocabulary",
                         sourceSchema = "cem_v3_0_0_20210308_translated")

withr::defer({
  backend$finalize()
})

test_that("summary works", {
  sinfo <- backend$getCemSourceInfo()
  expect_data_frame(sinfo)
})

test_that("get exposure and outcome control concepts", {
  srchOutcomeConceptSet <- data.frame(conceptId = c(4110112), includeDescendants = c(1), isExcluded = c(0))
  ingredientConcepts <- backend$getConditionEvidenceSummary(srchOutcomeConceptSet)
  expect_data_frame(ingredientConcepts)
  expect_true(nrow(ingredientConcepts) > 100)

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
  outcomeConcepts <- backend$getIngredientEvidenceSummary(srchIngredientSet)
  expect_data_frame(outcomeConcepts)
  expect_true(nrow(outcomeConcepts) > 100)
})