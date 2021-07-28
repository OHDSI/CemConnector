## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
devtools::load_all()
sqlidb <- tempfile(fileext = ".sqlite")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = sqlidb)
.loadCemTestFixtures(connectionDetails)
cemConnection <- CemConnector::CemDatabaseBackend$new(connectionDetails = connectionDetails,
                                                      vocabularySchema = "main",
                                                      cemSchema = "main",
                                                      sourceSchema = "main")

## ---- eval = FALSE------------------------------------------------------------
#  
#  cemConnection <- CemConnector::CemWebApiBackend(apiUrl = "https://cem.ohdsi.org/")

## ---- eval = FALSE------------------------------------------------------------
#  
#  connectionDetails <- DatabaseConnector::createConnectionDetails(user = "mydbusername",
#                                                                  server = "myserver/foo",
#                                                                  dbms = "redshift",
#                                                                  password = "mysecret")
#  
#  cemConnection <- CemConnector::CemDatabaseBackend$new(connectionDetails = connectionDetails,
#                                                        vocabularySchema = "vocabulary",
#                                                        cemSchema = "cem_v2",
#                                                        sourceSchema = "cem_v2_source")

## -----------------------------------------------------------------------------
cemConnection$getCemSourceInfo()

## -----------------------------------------------------------------------------
ingredientConceptSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
outcomeConcepts <- cemConnection$getSuggestedControlCondtions(ingredientConceptSet)
outcomeConcepts

## -----------------------------------------------------------------------------
conditionConceptSet <- data.frame(conceptId = c(440383), includeDescendants = c(1), isExcluded = c(0))
outcomeConcepts <- cemConnection$getSuggestedControlIngredients(conditionConceptSet)
outcomeConcepts

## -----------------------------------------------------------------------------
ingredientConceptSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
outcomeConcepts <- cemConnection$getIngredientEvidenceSummary(ingredientConceptSet)

## -----------------------------------------------------------------------------
parentIngredientSet <- data.frame(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0))

## -----------------------------------------------------------------------------
outcomeConcepts <- cemConnection$getIngredientEvidenceSummary(parentIngredientSet)
outcomeConcepts

## -----------------------------------------------------------------------------
conditionEvidence <- cemConnection$getIngredientEvidence(parentIngredientSet)
conditionEvidence

## -----------------------------------------------------------------------------
conditionConceptSet <- data.frame(conceptId = c(433440), includeDescendants = c(1), isExcluded = c(0))
cemConnection$getConditionEvidenceSummary(ingredientConceptSet)

## -----------------------------------------------------------------------------
cemConnection$getConditionEvidence(ingredientConceptSet)

## -----------------------------------------------------------------------------
minorDepressionConceptSet <- data.frame(conceptId = c(440383), includeDescendants = c(0), isExcluded = c(0))
cemConnection$getConditionEvidence(minorDepressionConceptSet, siblingLookupLevels = 1)

## -----------------------------------------------------------------------------
conditionConceptSet <- data.frame(conceptId = c(433440), includeDescendants = c(1), isExcluded = c(0))
ingredientConceptSet <- data.frame(conceptId = c(1201620), includeDescendants = c(1), isExcluded = c(0))
cemConnection$getRelationships(conditionConceptSet = conditionConceptSet, ingredientConceptSet = ingredientConceptSet)

