backend <- CemDatabaseBackend$new(connectionDetails,
                                  cemSchema = cemTestSchema,
                                  vocabularySchema = vocabularySchema,
                                  sourceSchema = sourceInfoSchema)

test_that("module loads", {
  shiny::testServer(ceExplorerModule, args = list(backend = backend,
                                                  ingredientConceptInput = shiny::reactive({ data.frame(conceptId = 21604296, includeDescendants = 1, isExcluded = 0) }),
                                                  conditionConceptInput = shiny::reactive({ data.frame(conceptId = 4149320, includeDescendants = 1, isExcluded = 0) }),
                                                  siblingLookupLevelsInput = shiny::reactive({ 1 })), {
    ingConcept <- ingredientConceptInput()
    expect_data_frame(ingConcept)
    condConcept <- conditionConceptInput()
    expect_data_frame(condConcept)
    expect_equal(siblingLookupLevelsInput(), 1)
    expect_true(output$errorMessage == "")

    relationships <- getRelationships()
    expect_true(output$errorMessage == "")
    expect_data_frame(relationships)
  })
})
test_that("explorer loads", {

  .GlobalEnv$backend <- backend
  shiny::testServer(ceExplorerDashboardServer, {
    ingredientCsvStr <- "conceptId,includeDescendants,isExcluded\n21604296,1,0"
    conditionCsvStr <- "conceptId,includeDescendants,isExcluded\n4149320,1,0"
    session$setInputs(siblingLookupLevels = 1, conditionConcept = conditionCsvStr, ingredientConcept = ingredientCsvStr)
    # test reactives
    ingConcept <- ingredientConceptInput()
    expect_data_frame(ingConcept)
    condConcept <- conditionConceptInput()
    expect_data_frame(condConcept)
    expect_equal(siblingLookupLevelsInput(), 1)
    expect_data_frame(getSourceInfo())
    expect_list(output$sourceInfo)

    session$setInputs(conditionConcept = "", ingredientConcept = "")
  })
})