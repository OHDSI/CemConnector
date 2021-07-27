## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
devtools::load_all()
sqlidb <- tempfile(fileext = ".sqlite")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms="sqlite", server = sqlidb)

.loadCemTestFixtures(connectionDetails)

## -----------------------------------------------------------------------------

cemConnection <- CemConnector::CemDatabaseBackend$new(connectionDetails = connectionDetails,
                                                      vocabularySchema = "main",
                                                      cemSchema = "main",
                                                      sourceSchema = "main")

## -----------------------------------------------------------------------------
ingreditentConceptSet <- data.frame(conceptId = c(1201620), includeDescendants = c(0), isExcluded = c(0))

## -----------------------------------------------------------------------------
parentIngredientSet <- data.frame(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0))

## -----------------------------------------------------------------------------
outcomeConcepts <- cemConnection$getIngredientEvidenceSummary(ingreditentConceptSet)
outcomeConcepts

## -----------------------------------------------------------------------------
outcomeConcepts <- cemConnection$getIngredientEvidenceSummary(ingreditentConceptSet)

## -----------------------------------------------------------------------------
outcomeConcepts <- cemConnection$getSuggestedControlCondtions(ingreditentConceptSet)
outcomeConcepts

