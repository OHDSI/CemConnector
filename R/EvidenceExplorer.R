# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of CemConnector
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' CE Explorer module
#' @description
#' Shiny Module for integration of evidence for conceptsets in to shiny applications
#'
#' @param id string - unique namespace for module
#' @param backend CemConnector backend (database or Api URL)
#' @param ingredientConceptInput shiny::reactive that returns data.frame with headers conceptId, includeDescendants, isExcluded
#' @param conditionConceptInput shiny::reactive that returns data.frame with headers conceptId, includeDescendants, isExcluded
#' @param siblingLookupLevelsInput shiny::reactive that returns positive integer for sibling levels to lookup for condition concept mappings to CEM
#' @importFrom utils write.csv
#' @importFrom dplyr select mutate %>%
#' @export
ceExplorerModule <- function(id,
                             backend,
                             ingredientConceptInput = shiny::reactive({
                               data.frame()
                             }),
                             conditionConceptInput = shiny::reactive({
                               data.frame()
                             }),
                             siblingLookupLevelsInput = shiny::reactive({
                               0
                             })) {
  checkmate::assert_class(backend, "AbstractCemBackend")
  checkmate::assert_class(ingredientConceptInput, "reactive")
  checkmate::assert_class(conditionConceptInput, "reactive")
  checkmate::assert_class(siblingLookupLevelsInput, "reactive")

  cemExplorerServer <- function(input, output, session) {
    output$errorMessage <- shiny::renderText("")
    getRelationships <- shiny::reactive({
      relationships <- data.frame()
      ingredientConceptSet <- ingredientConceptInput()
      conditionConceptSet <- conditionConceptInput()
      siblingLookupLevels <- as.integer(siblingLookupLevelsInput())
      if (!(checkmate::check_class(ingredientConceptSet, "data.frame") |
        checkmate::check_class(conditionConceptSet, "data.frame"))) {
        output$errorMessage <- shiny::renderText("Invalid concept sets defined")
        return(relationships)
      }

      if (nrow(conditionConceptSet) > 0 & nrow(ingredientConceptSet) > 0) {
        relationships <- backend$getRelationships(ingredientConceptSet, conditionConceptSet, siblingLookupLevels)
      } else if (nrow(ingredientConceptSet) > 0) {
        relationships <- backend$getIngredientEvidence(ingredientConceptSet)
      } else if (nrow(conditionConceptSet) > 0) {
        relationships <- backend$getConditionEvidence(conditionConceptSet, siblingLookupLevels)
      } else {
        output$errorMessage <- shiny::renderText("No concept sets defined")
        return(relationships)
      }

      if (nrow(relationships) == 0) {
        output$errorMessage <- shiny::renderText("No evidence found for concept set mapping")
      }
      output$errorMessage <- shiny::renderText("")
      return(relationships)
    })

    output$evidenceTable <- shiny::renderDataTable({
      shiny::withProgress(
        message = "Finding evidence related to concept set(s)",
        detail = "querying common evidence model",
        {
          rel <- getRelationships()
        }
      )

      if (nrow(rel) == 0) {
        return(rel)
      }

      rel %>%
        dplyr::mutate(
          ingredientConceptName = conceptName1,
          ingredientConceptId = conceptId1,
          conditionConceptName = conceptName2,
          conditionConceptId = conceptId2
        ) %>%
        dplyr::select(
          ingredientConceptName,
          ingredientConceptId,
          conditionConceptName,
          conditionConceptId,
          sourceId,
          relationshipId,
          evidenceType,
          statisticValue
        )
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0("ceExplorer_evidence.csv")
      },
      content = function(file) {
        write.csv(getRelationships(), file, row.names = FALSE)
      }
    )
  }

  shiny::moduleServer(id, cemExplorerServer)
}

#' CE Explorer module
#' @description
#' Shiny Module for integration of evidence for conceptsets in to shiny applications
#'
#' @param id string - unique namespace for module. Must match call to ceExplorerModule
#' @importFrom shinycssloaders withSpinner
#' @export
ceExplorerModuleUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::textOutput(ns("errorMessage")),
    shinycssloaders::withSpinner(shiny::dataTableOutput(ns("evidenceTable"))),
    shiny::downloadButton(ns("downloadData"))
  )
}


#' Negative control selection  utility
#' @description
#' Shiny Module for integration of evidence for selecting negative controls conceptsets in to shiny applications
#'
#' @param id string - unique namespace for module
#' @param backend CemConnector backend (database or Api URL)
#' @param conceptInput shiny::reactive that returns data.frame with headers conceptId, includeDescendants, isExcluded
#' @param siblingLookupLevelsInput shiny::reactive that returns positive integer for sibling levels to lookup for condition concept mappings to CEM
#' @param nControls shiny::reactive that returns positive integer for number of controls to get
#' @param isOutcomeSearch shiny::reactive that returns boolean - is this an indication, in which case search for disease concepts. Otherwise, searches for ingredients
#' @importFrom utils write.csv
#' @export
negativeControlSelectorModule <- function(id,
                                          backend,
                                          conceptInput = NULL,
                                          siblingLookupLevelsInput = shiny::reactive({
                                            0
                                          }),
                                          isOutcomeSearch = shiny::reactive({
                                            TRUE
                                          }),
                                          nControls = shiny::reactive({
                                            100
                                          })) {
  checkmate::assert_class(backend, "AbstractCemBackend")
  checkmate::assert_class(conceptInput, "reactive")
  checkmate::assert_class(siblingLookupLevelsInput, "reactive")
  checkmate::assert_class(nControls, "reactive")

  serverFunc <- function(input, output, session) {
    output$errorMessage <- shiny::renderText("")
    getControls <- shiny::reactive({
      inputConceptSet <- conceptInput()
      if (!(checkmate::test_data_frame(inputConceptSet, min.rows = 1))) {
        output$errorMessage <- shiny::renderText("Invalid concept set")
        return(data.frame())
      }

      if (isOutcomeSearch()) {
        return(backend$getSuggestedControlCondtions(inputConceptSet, nControls = nControls()))
      }
      return(backend$getSuggestedControlIngredients(inputConceptSet,
        nControls = nControls(),
        siblingLookupLevels = siblingLookupLevelsInput()
      ))
    })

    output$controlsTable <- shiny::renderDataTable({
      shiny::withProgress(
        message = "Loading negative controls",
        detail = "querying common evidence model",
        {
          dt <- getControls()
        }
      )
      return(dt)
    })


    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0("ceExplorer_negative_controls.csv")
      },
      content = function(file) {
        write.csv(getControls(), file, row.names = FALSE)
      }
    )
  }

  shiny::moduleServer(id, serverFunc)
}

#' negative control module ui
#' @description
#' Shiny Module for integration of negative control search for conceptsets in to shiny applications
#'
#' @param id string - unique namespace for module. Must match call to ceExplorerModule
#' @importFrom shinycssloaders withSpinner
#' @export
negativeControlSelectorUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::textOutput(ns("errorMessage")),
    shinycssloaders::withSpinner(shiny::dataTableOutput(ns("controlsTable"))),
    shiny::downloadButton(ns("downloadData"))
  )
}

#' @importFrom utils packageVersion read.csv
ceExplorerUi <- function(request) {
  inputArea <- shinydashboard::box(
    title = "Explore evidence",
    width = 12,
    shiny::p("Use CemConnector to explore pharmacovigalence evidence related to standard (RxNorm or ATC class) ingredients,
     standard conditions (SNOMED concepts) or a combination to find evidence related to these pairs."),
    shiny::selectInput("conceptInputType", "Input type", c("json", "list", "csv")),
    shiny::p(shiny::textOutput("conceptInputHelpTxt")),
    shiny::textAreaInput("ingredientConcept", label = "Ingredient concept set"),
    shiny::textAreaInput("conditionConcept", label = "Condition concept set"),
    shiny::selectInput("siblingLookupLevels", label = "Condition Sibling Lookup Levels", 0:5, selected = 0),
    shiny::p("If concept matches are poor, condition concepts may be too specfic, consdier looking for siblings.
                                   Siblings are related terms that are shared by a common parent of any specified search terms.
                                   This will lead to increased matches, but will likely result in many more false positives.
                                   ")
  )

  explorerTab <- shiny::fluidRow(
    inputArea,
    shinydashboard::box(
      width = 12,
      ceExplorerModuleUi("explorer")
    )
  )

  controlsInputArea <- shinydashboard::box(
    title = "Negative control suggestion",
    width = 12,
    shiny::p("Use CemConnector to get a set of negative controls for a given conceptset of interest."),
    shiny::selectInput("conceptInputTypeNc", "Input type", c("json", "list", "csv"), selected = "list"),
    shiny::p(shiny::textOutput("conceptInputHelpTxtNc")),
    shiny::textAreaInput("conceptSetNc", label = "Concept set", value = "1201620"),
    shiny::selectInput("siblingLookupLevelsNc", label = "Condition Sibling Lookup Levels", 0:5, selected = 0),
    shiny::p("When searching for ingredient exposure controls, If concept matches are poor, condition concepts may be too specfic, consdier looking for siblings"),
    shiny::checkboxInput("searchOutcomeControls", "Search for outcome (condition) controls", value = TRUE),
    shiny::selectInput("nControls", label = "Number of suggestsions", c(10, 20, 50, 100, 500, 5000), selected = 100)
  )

  controlsTab <- shiny::fluidRow(
    controlsInputArea,
    shinydashboard::box(
      width = 12,
      negativeControlSelectorUi("controls")
    )
  )

  aboutCemBox <- shinydashboard::box(
    title = "About The Common Evidence Model",
    shiny::p("The Common Evidence Model (CEM) combines many data sources in to a standard format to provide a standard resource for PharmaCovigilance activities."),
    shiny::p("Evidence uses OMOP Standard Vocabularies at the RXNorm and SNOMED levels"),
    shiny::p("For more information visit:"),
    shiny::a("https://github.com/OHDSI/CommonEvidenceModel/wiki")
  )

  cemSourcesBox <- shinydashboard::box(title = "Evidence Sources", width = 12, shiny::dataTableOutput("sourceInfo"))

  cemConnectorInfoBox <- shinydashboard::box(
    title = "CemConnector",
    width = 6,
    shiny::p("CE Explorer is part of the CemConnector package and is open source under the Apaceh License version 2.0. Latest package available at:"),
    shiny::p(shiny::a("https://github.com/OHDSI/CemConnector")),
    shiny::p("Server package version:"),
    shiny::p(shiny::textOutput("serverVersion")),
    shiny::p("Client package version:"),
    shiny::p(paste0(utils::packageVersion("CemConnector")))
  )


  aboutTab <- shiny::fluidRow(aboutCemBox, cemConnectorInfoBox, cemSourcesBox)
  body <- shinydashboard::dashboardBody(shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "About", aboutTab),
    shinydashboard::tabItem(tabName = "Controls", controlsTab),
    shinydashboard::tabItem(tabName = "Explore", explorerTab)
  ))

  sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
    shinydashboard::menuItem("About", tabName = "About", icon = shiny::icon("list-alt")),
    shinydashboard::menuItem("Explore Evidence", tabName = "Explore", icon = shiny::icon("table")),
    shinydashboard::menuItem("Negative controls", tabName = "Controls", icon = shiny::icon("search")),
    shiny::bookmarkButton()
  ))

  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "CE Explorer"),
    sidebar,
    body
  )
}

.readJsonString <- function(text) {
  definition <- jsonlite::fromJSON(text)
  conceptSet <- definition$items %>%
    dplyr::mutate(conceptId = definition$items$concept$CONCEPT_ID) %>%
    dplyr::select(conceptId, includeDescendants, isExcluded)
  return(conceptSet)
}

.readCommaSeparatedList <- function(text) {
  ids <- sapply(stringr::str_split(text, pattern = ","), as.integer)
  if (any(is.na(ids)) | !any(is.integer(ids))) {
    stop("Only valid integers can be used")
  }

  conceptSet <- data.frame(
    conceptId = ids,
    includeDescendants = 1,
    isExcluded = 0
  )
  return(conceptSet)
}

parseConceptInput <- function(conceptSetDefinition, inputType) {
  rda <- data.frame()
  tryCatch(
    {
      rda <- switch(inputType,
        "list" = .readCommaSeparatedList(conceptSetDefinition),
        "json" = .readJsonString(conceptSetDefinition),
        "csv" =  utils::read.csv(text = conceptSetDefinition)
      )
    },
    error = function(err) { }
  )
  rda
}

getInputHelpText <- function(inputType) {
  switch(inputType,
    "json" = "Copy and paste a json string from an atlas concept set definition export.",
    "csv" = "Manually input csv required headers: conceptId, includeDescendants, isExcluded",
    "list" = "Insert comma separated set of values. All will be included in search, descedants will also be searched automatically."
  )
}

ceExplorerDashboardServer <- function(input, output, session) {
  env <- globalenv()
  backend <- env$backend
  checkmate::assertClass(backend, "AbstractCemBackend")

  output$conceptInputHelpTxt <- shiny::renderText({
    getInputHelpText(input$conceptInputType)
  })

  output$conceptInputHelpTxtNc <- shiny::renderText({
    getInputHelpText(input$conceptInputTypeNc)
  })

  ingredientConceptInput <- shiny::reactive({
    parseConceptInput(input$ingredientConcept, input$conceptInputType)
  })
  conditionConceptInput <- shiny::reactive({
    parseConceptInput(input$conditionConcept, input$conceptInputType)
  })
  siblingLookupLevelsInput <- shiny::reactive({
    input$siblingLookupLevels
  })
  getSourceInfo <- shiny::reactive({
    backend$getCemSourceInfo()
  })
  output$sourceInfo <- shiny::renderDataTable({
    getSourceInfo()
  })

  output$serverVersion <- shiny::renderText({
    paste0(backend$getVersion())
  })

  ceModuleServer <- ceExplorerModule("explorer",
    backend,
    ingredientConceptInput = ingredientConceptInput,
    conditionConceptInput = conditionConceptInput,
    siblingLookupLevelsInput = siblingLookupLevelsInput
  )

  conceptInputNc <- shiny::reactive({
    parseConceptInput(input$conceptSetNc, input$conceptInputTypeNc)
  })
  siblingLookupLevelsInputNc <- shiny::reactive({
    input$siblingLookupLevelsNc
  })
  searchOutcomeControls <- shiny::reactive({
    input$searchOutcomeControls
  })
  nControls <- shiny::reactive({
    input$nControls
  })
  ncModuleServer <- negativeControlSelectorModule("controls",
    backend,
    conceptInput = conceptInputNc,
    isOutcomeSearch = searchOutcomeControls,
    nControls = nControls,
    siblingLookupLevelsInput = siblingLookupLevelsInputNc
  )
}

#' CE Explorer shiny application
#' @description
#' Shiny App for exploring common evidence model
#'
#' @param apiUrl string - url for CemConnector API or NULL
#' @param connectionDetails DatabaseConnector::connectionDetails instance for CEM
#' @param usePooledConnection - use pooled connections (database model only)
#' @param ... param list paased to CemDatabaseBaackend$new
#' @export
launchCeExplorer <- function(apiUrl = "https://cem.ohdsi.org",
                             connectionDetails = NULL,
                             usePooledConnection = TRUE,
                             ...) {
  environment <- globalenv()
  checkmate::assert_class(connectionDetails, "connectionDetails", null.ok = TRUE)
  checkmate::assert_string(apiUrl, null.ok = TRUE)

  if (is.null(apiUrl) & is.null(connectionDetails)) {
    stop("Must set either api url or CEM connection sources")
  } else if (!is.null(connectionDetails)) {
    environment$backend <- CemDatabaseBackend$new(
      connectionDetails = connectionDetails,
      usePooledConnection = usePooledConnection, ...
    )
  } else {
    environment$backend <- CemWebApiBackend$new(apiUrl = apiUrl)
  }

  shiny::shinyApp(server = ceExplorerDashboardServer, ceExplorerUi, enableBookmarking = "url", onStart = function() {
    shiny::onStop(function() {
      writeLines("Closing connection")
      environment$backend$finalize()
    })
  })
}
