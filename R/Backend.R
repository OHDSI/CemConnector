.loadSqlFile <- function(sqlFilename) {
  pathToSql <- system.file(paste("sql/sql_server"),
                           sqlFilename,
                           package = "CEMConnector",
                           mustWork = TRUE)
  sql <- SqlRender::readSql(pathToSql)
}

#' @export
DBBackend <- R6Class(
  "DBBackend",
  public = list(
    connection = NULL,
    cemSchema = NULL,
    vocabularySchema = NULL,
    sourceSchema = NULL,
    initialize = function(connectionDetails,
                          cemSchema,
                          vocabularySchema,
                          sourceSchema,
                          usePooledConnection = FALSE) {
      if (usePooledConnection) {
        self$connection <- PooledConnection$new(connectionDetails)
      } else {
        self$connection <- Connection$new(connectionDetails)
      }

      self$cemSchema <- cemSchema
      self$vocabularySchema <- vocabularySchema
      self$sourceSchema <- sourceSchema
    },

    finalize = function() {
      self$connection$finalize()
    },

      #' @description
      #' Reutrns set of ingredient concepts for a given conceptset of outcomes
      #'
    getConditionEvidenceSummary = function(conditionConceptSet,
                                           siblingLookupLevels = 0) {

      checkmate::assert_data_frame(conditionConceptSet)
      checkmate::checkNames(names(conditionConceptSet), must.include = c("includeDescendants", "conceptId", "isExcluded"))
      conditionConceptDesc <- conditionConceptSet[conditionConceptSet$isExcluded == 0 & conditionConceptSet$includeDescendants == 1,]$conceptId
      conditionConceptNoDesc <- conditionConceptSet[conditionConceptSet$isExcluded == 0 & conditionConceptSet$includeDescendants == 0,]$conceptId
      sql <- .loadSqlFile("getConditionEvidenceSummary.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              use_siblings = siblingLookupLevels,
                              condition_concept_desc = conditionConceptDesc,
                              condition_concept_no_desc = conditionConceptNoDesc)
    },

      #' @description
      #' Reutrns set of outcome concepts for a given conceptset of ingredients/exposures
      #'
    getIngredientEvidenceSummary = function(ingredientConceptSet,
                                            parentLookUpLevels = 0) {
      checkmate::assert_data_frame(ingredientConceptSet)
      checkmate::checkNames(names(ingredientConceptSet), must.include = c("includeDescendants", "conceptId", "isExcluded"))
      conditionConceptDesc <- ingredientConceptSet[ingredientConceptSet$isExcluded == 0 & ingredientConceptSet$includeDescendants == 1,]$conceptId
      conditionConceptNoDesc <- ingredientConceptSet[ingredientConceptSet$isExcluded == 0 & ingredientConceptSet$includeDescendants == 0,]$conceptId

      sql <- .loadSqlFile("getIngredientEvidenceSummary.sql")
      self$connection$queryDb(sql,
                              vocabulary = self$vocabularySchema,
                              cem_schema = self$cemSchema,
                              concept_desc = conditionConceptDesc,
                              concept_no_desc = conditionConceptNoDesc)

    },

    getCemSourceInfo = function() {
      return(self$connection$queryDb("SELECT * FROM @schema.source", schema = self$sourceSchema))
    }
  )
)
