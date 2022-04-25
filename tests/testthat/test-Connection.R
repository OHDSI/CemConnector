test_that("Connection factory", {
  webBackend <- createCemConnection(apiUrl = apiUrl)
  expect_class(webBackend, "CemWebApiBackend")

  dbBackend <- createCemConnection(
    connectionDetails = connectionDetails,
    cemDatabaseSchema = cemTestSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    sourceDatabaseSchema = sourceInfoSchema
  )

  expect_class(dbBackend, "CemDatabaseBackend")
})
