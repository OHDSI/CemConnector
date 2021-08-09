test_that("Connection factory", {
  webBackend <- createCemConnection(apiUrl = apiUrl)
  expect_class(webBackend, "CemWebApiBackend")

  dbBackend <- createCemConnection(connectionDetails = connectionDetails,
                                   cemSchema = cemTestSchema,
                                   vocabularySchema = vocabularySchema,
                                   sourceSchema = sourceInfoSchema)

  expect_class(dbBackend, "CemDatabaseBackend")
})