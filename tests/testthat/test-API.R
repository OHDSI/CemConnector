port <- httpuv::randomPort(8000, 8080)

# Load API in separate process
serverStart <- function(apiPort, ...) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(...)
  api <- CEMConnector::loadApi(connectionDetails)
  api$run(port = apiPort)
}

apiProcess <- callr::r_bg(serverStart,
                          args = list(apiPort = port,
                                      server = Sys.getenv("CEM_DATABASE_SERVER"),
                                      user = dbUser,
                                      password = keyring::key_get(keyringService, username = dbUser),
                                      port = Sys.getenv("CEM_DATABASE_PORT"),
                                      dbms = Sys.getenv("CEM_DATABASE_DBMS"),
                                      extraSettings = Sys.getenv("CEM_DATABASE_EXTRA_SETTINGS")),
                          stderr = "plumbr_error.txt",
                          stdout = "plumbr_out.log")
withr::defer({
  apiProcess$kill()
}, testthat::teardown_env())


apiUrl <- paste0("http://localhost:", port)
Sys.sleep(5) # Allow time for process to start, needs to connect to database

test_that("Test api alive", {
  resp <- httr::GET(apiUrl)
  info <- httr::content(resp, as = "parsed")
  expect_equal(info$status, "alive")
})

test_that("Test api version", {
  resp <- httr::GET(paste0(apiUrl, "/version"))
  info <- httr::content(resp, as = "parsed")
  expect_equal(info$version, paste(packageVersion("CEMConnector")))
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

  expect_data_frame(data, min.rows = 10)
})

test_that("Test get ingredient evidence", {

  srchIngredientSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))

  resp <- httr::POST(paste0(apiUrl, "/ingredientEvidenceSummary"),
                     body = list(ingredientConceptSet = srchIngredientSet),
                     encode = "json")
  content <- httr::content(resp, as = "parsed")
  # Turn response in to data frame
  data <- do.call(rbind.data.frame, content$result)

  expect_data_frame(data, min.rows = 10)
})

test_that("Test get relationships", {

  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1,0), isExcluded = c(0))
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