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

#'@export
ceExplorerModule <- function(id,
                             backend,
                             ingredientConceptInput = reactive({ data.frame() }),
                             conditionConceptInput = reactive({ data.frame() }),
                             siblingLookupLevelsInput = reactive({ 0 })) {
  checkmate::assert_class(backend, "AbstractCemBackend")
  checkmate::assert_class(ingredientConceptInput, "reactive")
  checkmate::assert_class(conditionConceptInput, "reactive")
  checkmate::assert_class(siblingLookupLevelsInput, "reactive")


  cemExplorerServer <- function(input, output, session) {
    output$evidenceTable <- shiny::renderDataTable({
      output$errorMessage <- shiny::renderText("")
      ingredientConceptSet <- ingredientConceptInput()
      conditionConceptSet <- conditionConceptInput()
      siblingLookupLevels <- as.integer(siblingLookupLevelsInput())
      relationships <- data.frame()

      if (!(checkmate::check_class(ingredientConceptSet, "data.frame") |
        checkmate::check_class(conditionConceptSet, "data.frame"))) {
        output$errorMessage <- shiny::renderText("Invalid concept sets defined")
      } else {
        if (nrow(conditionConceptSet) > 0 & nrow(ingredientConceptSet) > 0) {
          relationships <- backend$getRelationships(ingredientConceptSet, conditionConceptSet, siblingLookupLevels)
        } else if (nrow(ingredientConceptSet) > 0) {
          relationships <- backend$getIngredientEvidence(ingredientConceptSet)
        } else if (nrow(conditionConceptSet) > 0) {
          relationships <- backend$getConditionEvidence(conditionConceptSet, siblingLookupLevels)
        } else {
          output$errorMessage <- shiny::renderText("No concept sets defined")
        }
      }
      relationships
    })
  }
  shiny::moduleServer(id, cemExplorerServer)
}

#'@export
ceExplorerModuleUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(shiny::div(shiny::textOutput(ns("errorMessage")),
                             shiny::dataTableOutput(ns("evidenceTable"))))
}

ceExplorerUi <- function(request) {

  inputArea <- shinydashboard::box(shiny::textAreaInput("ingredientConcept", label = "Ingredient concept set (csv)"),
                                   shiny::textAreaInput("conditionConcept", label = "Condition concept set (csv)"),
                                   shiny::selectInput("siblingLookupLevels", label = "Condition Sibling Lookup Levels", 0:5, selected = 0),
                                   shiny::actionButton("conceptSetRelations", "Find Evidence"))

  explorerTab <- shiny::fluidRow(inputArea, ceExplorerModuleUi("explorer"))
  aboutTab <- shiny::fluidRow()
  body <- shinydashboard::dashboardBody(shinydashboard::tabItems(shinydashboard::tabItem(tabName = "About", aboutTab),
                                                                 shinydashboard::tabItem(tabName = "Explore", explorerTab)))

  sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(shinydashboard::menuItem("About", tabName = "about", icon = icon("list-alt")),
                                                                          shinydashboard::menuItem("Explore", tabName = "Explore", icon = icon("table")),
                                                                          bookmarkButton()))

  shinydashboard::dashboardPage(shinydashboard::dashboardHeader(title = "CEExplorer"),
                                sidebar,
                                body)
}

.readCsvString <- function(text) {
  data <- data.frame()
  tryCatch({
    data <- read.csv(text = text)
  }, error = function(error) { })

  data
}

ceExplorerDashboardServer <- function(input, output, session) {
  ingredientConceptInput <- reactive({ .readCsvString(input$ingredientConcept) })
  conditionConceptInput <- reactive({ .readCsvString(input$conditionConcept) })
  siblingLookupLevelsInput <- reactive({ input$siblingLookupLevels })
  ceModuleServer <- ceExplorerModule("explorer",
                                     backend,
                                     ingredientConceptInput = ingredientConceptInput,
                                     conditionConceptInput = conditionConceptInput,
                                     siblingLookupLevelsInput = siblingLookupLevelsInput)
}

#'@export
launchEvidenceExplorer <- function(backend, environment = .GlobalEnv) {
  checkmate::assert_class(backend, "AbstractCemBackend")
  environment$backend <- backend

  shiny::shinyApp(server = ceExplorerDashboardServer, ceExplorerUi, enableBookmarking = "url", onStart = function() {
    shiny::onStop(function() {
      writeLines("Closing connection")
      environment$backend$finalize()
    })
  })

}
