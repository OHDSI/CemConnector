backend <- CemWebApiBackend$new(apiUrl)

test_that("explorer loads", {
  .GlobalEnv$backend <- backend
  shiny::testServer(ceExplorerDashboardServer, {
    ingredientCsvStr <- "conceptId,includeDescendants,isExcluded\n21604296,1,0"
    conditionCsvStr <- "conceptId,includeDescendants,isExcluded\n4149320,1,0"
    session$setInputs(
      siblingLookupLevels = 1,
      conditionConcept = conditionCsvStr,
      ingredientConcept = ingredientCsvStr,
      nControls = 50,
      searchOutcomeControls = TRUE,
      siblingLookupLevelsNc = 1
    )
    # test reactives
    ingConcept <- ingredientConceptInput()
    expect_data_frame(ingConcept)
    condConcept <- conditionConceptInput()
    expect_data_frame(condConcept)
    expect_equal(siblingLookupLevelsInput(), 1)
    expect_equal(siblingLookupLevelsInputNc(), 1)
    expect_true(searchOutcomeControls())
    expect_equal(nControls(), 50)
    expect_data_frame(getSourceInfo())
    expect_list(output$sourceInfo)

    session$setInputs(conditionConcept = "", ingredientConcept = "")
  })
})

test_that("module loads and functions", {
  shiny::testServer(ceExplorerModule, args = list(
    backend = backend,
    ingredientConceptInput = shiny::reactive({
      data.frame(conceptId = 21604296, includeDescendants = 1, isExcluded = 0)
    }),
    conditionConceptInput = shiny::reactive({
      data.frame(conceptId = 4149320, includeDescendants = 1, isExcluded = 0)
    }),
    siblingLookupLevelsInput = shiny::reactive({
      1
    })
  ), {
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

  shiny::testServer(ceExplorerModule, args = list(backend = backend), {
    relationships <- getRelationships()
    expect_true(output$errorMessage == "No concept sets defined")
    expect_data_frame(relationships)
  })

  shiny::testServer(ceExplorerModule, args = list(
    backend = backend,
    conditionConceptInput = shiny::reactive({
      data.frame(conceptId = 4149320, includeDescendants = 1, isExcluded = 0)
    }),
    siblingLookupLevelsInput = shiny::reactive({
      1
    })
  ), {
    relationships <- getRelationships()
    expect_true(output$errorMessage == "")
    expect_data_frame(relationships)
  })

  shiny::testServer(ceExplorerModule, args = list(
    backend = backend,
    ingredientConceptInput = shiny::reactive({
      data.frame(conceptId = 21604296, includeDescendants = 1, isExcluded = 0)
    })
  ), {
    relationships <- getRelationships()
    expect_true(output$errorMessage == "")
    expect_data_frame(relationships)
  })
})

test_that("Ui functions execute", {
  # Just a test that the code runs, no real logic or UI testing
  expect_class(ceExplorerModuleUi("test"), "shiny.tag")
  expect_class(negativeControlSelectorUi("test2"), "shiny.tag")
  expect_class(ceExplorerUi(shiny::req(a = 1)), "shiny.tag")
})


test_that("Negative control module", {
  .GlobalEnv$backend <- backend
  shiny::testServer(negativeControlSelectorModule, args = list(
    backend = backend,
    conceptInput = shiny::reactive({
      data.frame(conceptId = 21604296, includeDescendants = 1, isExcluded = 0)
    }),
    siblingLookupLevelsInput = shiny::reactive({
      1
    })
  ), {
    ingConcept <- conceptInput()
    expect_equal(siblingLookupLevelsInput(), 1)
    expect_true(output$errorMessage == "")

    ctrls <- getControls()
    expect_true(output$errorMessage == "")
    expect_data_frame(ctrls)
  })

  shiny::testServer(negativeControlSelectorModule, args = list(
    backend = backend,
    conceptInput = shiny::reactive({
      data.frame(conceptId = 4149320, includeDescendants = 1, isExcluded = 0)
    }),
    isOutcomeSearch = shiny::reactive({
      FALSE
    }),
    siblingLookupLevelsInput = shiny::reactive({
      1
    })
  ), {
    ingConcept <- conceptInput()
    expect_equal(siblingLookupLevelsInput(), 1)
    expect_true(output$errorMessage == "")

    ctrls <- getControls()
    expect_true(output$errorMessage == "")
    expect_data_frame(ctrls)
  })

  shiny::testServer(negativeControlSelectorModule, args = list(
    backend = backend,
    conceptInput = shiny::reactive({
      NULL
    })
  ), {
    ctrls <- getControls()
    expect_true(output$errorMessage == "Invalid concept set")
    expect_data_frame(ctrls)
  })
})
