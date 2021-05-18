Connection <- R6Class(
  "Connection",
  public = list(
    connectionDetails = NULL,
    con = NULL,
    isActive = FALSE,
    initialize = function(connectionDetails) {
      checkmate::assert_class(connectionDetails, "connectionDetails")
      self$connectionDetails <- connectionDetails
      self$initConnection()
    },

    renderTranslateSql = function(query, ...) {
      sql <- SqlRender::render(sql = query, ...)
      SqlRender::translate(sql, targetDialect = self$connectionDetails$dbms)
    },

    initConnection = function() {
      if(self$isActive) {
        warning("Closing existing connection")
        self$closeConnection()
      }
      self$con <- DatabaseConnector::connect(connectionDetails = self$connectionDetails)
      self$isActive <- TRUE
    },


    getConnection = function() {
      return(self$con)
    },

    closeConnection = function() {
      if (DBI::dbIsValid(dbObj = self$con)) {
        DatabaseConnector::disconnect(self$con)
      }
      self$isActive <- FALSE
    },

    finalize = function() {
      if (self$isActive & DBI::dbIsValid(dbObj = self$con)) {
        self$closeConnection()
      }
    },

    queryDb = function(query, snakeCaseToCamelCase = TRUE, ...) {
      sql <- self$renderTranslateSql(query, ...)
      tryCatch({
        data <- DatabaseConnector::querySql(self$con, sql, snakeCaseToCamelCase = snakeCaseToCamelCase)
      }, error = function(e, ...) {
        ParallelLogger::logError(e)
        if (self$connectionDetails$dbms %in% c("postgresql", "redshift")) {
          DatabaseConnector::dbExecute(self$con, "ABORT;")
        }
      })
      return(data)
    }
  )
)

PooledConnection <- R6Class(
  "PooledConnection",
  inherit = Connection,
  public = list(

    initConnection = function() {
      if(self$isActive) {
        warning("Closing existing connection")
        self$closeConnection()
      }

      self$con <- pool::dbPool(
        drv = DatabaseConnector::DatabaseConnectorDriver(),
        dbms = self$connectionDetails$dbms,
        server = self$connectionDetails$server(),
        port = self$connectionDetails$port(),
        user = self$connectionDetails$user(),
        password = self$connectionDetails$password()
      )
      self$isActive <- TRUE
    },

    closeConnection = function() {
      if (DBI::dbIsValid(dbObj = self$con)) {
        pool::poolClose(pool = self$con)
      }
      self$isActive <- FALSE
    },

    queryDb = function(query, snakeCaseToCamelCase = TRUE, ...) {
      sql <- self$renderTranslateSql(query, ...)
      data <- DatabaseConnector::dbGetQuery(self$con, sql)
      if (snakeCaseToCamelCase) {
        colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
      } else {
        colnames(data) <- toupper(colnames(data))
      }
      return(data)
    }
  )
)
