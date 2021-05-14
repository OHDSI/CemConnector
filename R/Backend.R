DBBackend <- R6Class(
  "DBBackend",
  public = list(
    connection = NULL,
    cemSchema = NULL,
    vocabularySchema = NULL,

    initialize = function(connectionDetails,
                          cemSchema,
                          vocabularySchema,
                          usePooledConnection = FALSE) {
      if (usePooledConnection) {
        self$connection = PooledConnection$new(connectionDetails)
      } else {
        self$connection = Connection$new(connectionDetails)
      }

      self$cemSchema = cemSchema
      self$vocabularySchema = vocabularySchema
    },
    finalize = function() {
      self$connection$finalize()
    },


    #' @description
    #' Reutrns set of outcome concepts for a given conceptset of ingredients/exposures
    #'
    getOutcomeEvidenceSummaryByConceptSet = function(conceptSet,
                                                     parentLookUpLevels = 0,
                                                     returnVocabNames = TRUE,
                                                     warnOnNonStandardConcepts = TRUE) {

    },

    #' @description
    #' Reutrns set of outcome concepts for a given conceptset of ingredients/exposures
    #'
    getExposureEvidenceSummaryByConceptSet = function(conceptSet,
                                                      parentLookUpLevels = 0,
                                                      returnVocabNames = TRUE,
                                                      warnOnNonStandardConcepts = TRUE) {

    }
  )
)
