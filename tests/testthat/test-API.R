test_that("load function works", {
  tenv <- environment()
  api <- loadApi(connectionDetails,
                 cemSchema = cemTestSchema,
                 vocabularySchema = vocabularySchema,
                 sourceSchema = sourceInfoSchema,
                 envir = tenv)
  expect_s3_class(api, "Plumber")
  # Test load endpoints
  expect_class(tenv$cemBackendApi, "CemDatabaseBackend")
  tenv$cemBackendApi$finalize()
})

test_that("Test api alive", {
  resp <- httr::GET(apiUrl)
  info <- httr::content(resp, as = "parsed")
  expect_equal(info$status, "alive")
})

test_that("Test api version", {
  resp <- httr::GET(paste0(apiUrl, "/version"))
  info <- httr::content(resp, as = "parsed")
  expect_equal(info$version, paste(packageVersion("CemConnector")))
})

test_that("Test get source info", {
  resp <- httr::GET(paste0(apiUrl, "/cemSourceInfo"))
  content <- httr::content(resp, as = "parsed")
  expect_true(length(content$result) > 5)
})

test_that("Test get condition evidence", {
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))

  resp <- httr::POST(paste0(apiUrl, "/conditionEvidenceSummary"),
                     body = list(conditionConceptSet = srchOutcomeConceptSet, siblingLookupLevels = 1),
                     encode = "json")
  content <- httr::content(resp, as = "parsed")
  # Turn response in to data frame
  data <- do.call(rbind.data.frame, content$result)

  expect_data_frame(data, min.rows = 1)
})

test_that("Test get ingredient evidence", {
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))

  resp <- httr::POST(paste0(apiUrl, "/ingredientEvidenceSummary"),
                     body = list(ingredientConceptSet = srchIngredientSet),
                     encode = "json")
  content <- httr::content(resp, as = "parsed")
  # Turn response in to data frame
  data <- do.call(rbind.data.frame, content$result)

  expect_data_frame(data, min.rows = 100)
})

test_that("Test get outcome controls", {
  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))

  resp <- httr::POST(paste0(apiUrl, "/suggestedControlConditions"),
                     body = list(ingredientConceptSet = srchIngredientSet),
                     encode = "json")
  content <- httr::content(resp, as = "parsed")
  # Turn response in to data frame
  data <- do.call(rbind.data.frame, content$result)

  expect_data_frame(data, min.rows = 50, max.rows = 50)
})

test_that("Test get exposure controls", {
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))

  resp <- httr::POST(paste0(apiUrl, "/suggestedControlIngredients"),
                     body = list(conditionConceptSet = srchOutcomeConceptSet, siblingLookupLevels = 1),
                     encode = "json")
  content <- httr::content(resp, as = "parsed")
  # Turn response in to data frame
  data <- do.call(rbind.data.frame, content$result)

  expect_data_frame(data)
})

test_that("Test get relationships", {
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))
  srchOutcomeConceptSet <- data.frame(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0))

  resp <- httr::POST(paste0(apiUrl, "/ingredientEvidenceSummary"),
                     body = list(ingredientConceptSet = srchIngredientSet,
                                 conditionConceptSet = srchOutcomeConceptSet,
                                 conditionSiblingLookupLevels = 1),
                     encode = "json")

  content <- httr::content(resp, as = "parsed")
  # Turn response in to data frame
  data <- do.call(rbind.data.frame, content$result)

  expect_data_frame(data, min.rows = 10)
})