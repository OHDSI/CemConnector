test_that("Write controls to study package", {
  outpfile <- tempfile(fileext = ".csv")
  withr::defer(unlink(outpfile), testthat::teardown_env())

  backend <- createCemConnection(apiUrl = apiUrl)
  srchIngredientSet <- data.frame(conceptId = c(21604296, 1201620), includeDescendants = c(1, 0), isExcluded = c(0))

  suggestedControls <- backend$getSuggestedControlCondtions(srchIngredientSet)
  addControlsToStudyPackage(suggestedControls, fileName = outpfile, targetId = 1, comparatorId = 2)

  # check output
  ouputData <- read.csv(outpfile)
  checkmate::expect_names(names(ouputData),
                          must.include = c("targetId",
                                           "comparatorId",
                                           "outcomeId",
                                           "outcomeName",
                                           "type"))
  expect_equal(nrow(ouputData), nrow(suggestedControls))
  testRow <- data.frame(conceptId = 999989, conceptName = "Test concept")
  suggestedControls <- rbind(suggestedControls, testRow)

  # Check adding doesn't create duplicates
  addControlsToStudyPackage(suggestedControls, fileName = outpfile, targetId = 1, comparatorId = 2)
  ouputData2 <- read.csv(outpfile)
  expect_equal(nrow(ouputData2), nrow(ouputData) + 1)

  ouputData2$targetName <- "Foooo"
  write.csv(ouputData2, file = outpfile, row.names = FALSE, quote = FALSE)
  addControlsToStudyPackage(suggestedControls, fileName = outpfile, targetId = 3, comparatorId = 4)
  expect_warning(addControlsToStudyPackage(suggestedControls[1:5,], fileName = outpfile, targetId = 5, comparatorId = 6))
})