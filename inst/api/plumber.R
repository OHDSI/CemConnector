library(CEMConnector)
backend <- CEMDatabaseBackend$new(connectionDetails,
                                  cemSchema,
                                  vocabularySchema,
                                  sourceSchema,
                                  usePooledConnection = TRUE)

#* Get Webapi version info
#* @get /
#* @serializer unboxedJSON
function() {
  list(status = 'alive')
}

#* Get CEMConnector API version info
#* @get /version
#* @serializer unboxedJSON
function() {
  list(version = packageVersion("CEMConnector"))
}
