## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  baseUrl <- "https://cem.ohdsi.org/api/v1/"
#  cemConnection <- CemConnector::CemWebApiBackend$new(baseUrl)

## -----------------------------------------------------------------------------
ingreditentConceptSet <- data.frame(conceptId = c(1201620), includeDescendants = c(0), isExcluded = c(0))

## -----------------------------------------------------------------------------
parentIngredientSet <- data.frame(conceptId = c(21604296), includeDescendants = c(1), isExcluded = c(0))

## ---- eval = FALSE------------------------------------------------------------
#  outcomeConcepts <- backend$getIngredientEvidenceSummary(ingreditentConceptSet)
#  outcomeConcepts

