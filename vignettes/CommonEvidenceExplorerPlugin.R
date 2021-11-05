## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  baseUrl <- "https://cem.ohdsi.org/"
#  launchCeExplorer(apiUrl = baseUrl)

## ---- eval = FALSE------------------------------------------------------------
#  connectionDetails <- DatabaseConnector::createConnectionDetails("postgres", ...)
#  launchCeExplorer(connectionDetails = connectionDetails, cemSchema = "cem", sourceSchema = "cem_v3_sources", )

## ---- eval = FALSE------------------------------------------------------------
#  serverFunction <- function(input, output, session) {
#    backend <- CemConnector::CemWebApiBackend$new(apiUrl = "https://cem.ohdsi.org/")
#    # Define a reactive that returns a dataframe with standard ingredient concepts
#    ingredientConceptInput <- shiny::reactive({ tibble::tibble(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0)) })
#    # Define a reactive that returns a dataframe with standard condtion concepts
#    conditionConceptInput <- shiny::reactive({ tibble::tibble(conceptId = c(4149320), includeDescendants = c(1), isExcluded = c(0)) })
#    # Define a reactive that returns an integer that for looking up higher levels in the heirarchy
#    siblingLookupLevelsInput <- shiny::reactive({ 1 })
#  
#    # Call the explorer module
#    ceModuleServer <- ceExplorerModule("explorer", # This ID should be unqiue and match the call to ceExplorerModuleUi
#                                       backend,
#                                       ingredientConceptInput = ingredientConceptInput,
#                                       conditionConceptInput = conditionConceptInput,
#                                       siblingLookupLevelsInput = siblingLookupLevelsInput)
#  }
#  
#  uiFunction <- function () {
#    explorerTable <- ceExplorerModuleUi("exporer") # The id for the server module and ui should be the same
#    shiny::fluidPage(explorerTable)
#  }
#  
#  shiny::shinyApp(uiFunction, serverFunction)

