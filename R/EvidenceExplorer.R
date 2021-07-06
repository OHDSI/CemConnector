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
ceExplorerModule <- function(id, backend) {
  checkmate::assert_class(backend, "AbstractCemBackend")

  cemExplorerServer <- function(input, output, session) {
    shiny::observeEvent(input$conceptSetRelations, {
      output$errorMessage <- shiny::renderText("")
      output$displayResultsTable <- shiny::renderText("loaded")

      ingredientConceptSet <- NULL
      tryCatch({
        ingredientConceptSet <- read.csv(text = input$ingredientConceptInput)
      }, error = function (err) {
        ingredientConceptSet <<- data.frame()
      })


      conditionConceptSet <- NULL
      tryCatch({
        conditionConceptSet <- read.csv(text = input$conditionConceptSet)
      }, error = function (err) {
        conditionConceptSet <<- data.frame()
      })

      relationships <- data.frame()

      if (nrow(conditionConceptSet) > 0 & nrow(ingredientConceptSet) > 0) {
        relationships <- backend$getRelationships(ingredientConceptSet, conditionConceptSet, input$siblingLookupLevels)
      } else if (nrow(ingredientConceptSet) > 0 ) {
        relationships <- backend$getIngredientEvidence(ingredientConceptSet)
      } else if (nrow(conditionConceptSet) > 0 ) {
        relationships <- backend$getConditionEvidence(conditionConceptSet, input$siblingLookupLevels)
      } else {
        output$errorMessage <- shiny::renderText("Invalid concept sets defined")
      }

      output$evidenceTable <- shiny::renderDataTable(relationships)
    })
  }

  shiny::moduleServer(id, cemExplorerServer)
}

#'@export
ceExplorerModuleUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(shinydashboard::box(shiny::textAreaInput(ns("ingredientConceptInput"), label = "Ingredient concept set (csv)"),
                                      shiny::textAreaInput(ns("conditionConceptInput"), label = "Condition concept set (csv)"),
                                      shiny::selectInput(ns("siblingLookupLevels"), label = "Condition Sibling Lookup Levels", 0:5, selected = 0),
                                      shiny::actionButton(ns("conceptSetRelations"), "Find Evidence"),
                                      shiny::textOutput(ns("errorMessage")),
                                      shiny::dataTableOutput(ns("evidenceTable"))))
}

ceExplorerUi <- function(request) {

  explorerTab <- shiny::fluidRow(shinydashboard::box(ceExplorerModuleUi("explorer")))
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

ceExplorerDashboardServer <- function(input, output, session) {

  ceModuleServer <- ceExplorerModule("explorer", backend)
}

#'@export
launchEvidenceExplorer <- function(backend, environment = .GlobalEnv) {
  checkmate::assert_class(backend, "AbstractCemBackend")
  environment$backend <- backend
  # TODO:: launch shiny app
  shiny::shinyApp(server = ceExplorerDashboardServer, ceExplorerUi, enableBookmarking = "url", onStart = function() {
    shiny::onStop(function() {
      writeLines("Closing connection")
      environment$backend$finalize()
    })
  })

}
