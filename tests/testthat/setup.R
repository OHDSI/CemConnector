# Load API in separate process
serverStart <- function(pipe, apiPort, cemSchema, vocabularySchema, sourceSchema, ...) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(...)

  tryCatch({
    api <- CemConnector::loadApi(connectionDetails,
                               cemSchema = cemSchema,
                               vocabularySchema = vocabularySchema,
                               sourceSchema = sourceSchema)
    writeLines("API LOADED", con = pipe)
    api$run(port = apiPort)
  }, error = function(err) {
    writeLines("API FAILED", con = pipe)
    writeLines(err, con = pipe)
  })
}

jDriverPath <- tempfile("jDriverPath")
dir.create(jDriverPath)
DatabaseConnector::downloadJdbcDrivers(Sys.getenv("CEM_DATABASE_DBMS"), pathToDriver = jDriverPath)
connectionParams <- list(server = Sys.getenv("CEM_DATABASE_SERVER"),
                         user = Sys.getenv("CEM_DATABASE_USER"),
                         password = Sys.getenv("CEM_DATABASE_PASSWORD"),
                         port = Sys.getenv("CEM_DATABASE_PORT"),
                         dbms = Sys.getenv("CEM_DATABASE_DBMS"),
                         extraSettings = Sys.getenv("CEM_DATABASE_EXTRA_SETTINGS"),
                         pathToDriver = jDriverPath)

connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, connectionParams)

cemTestSchema <- Sys.getenv("CEM_DATABASE_SCHEMA")
vocabularySchema <- Sys.getenv("CEM_DATABASE_VOCAB_SCHEMA")
sourceInfoSchema <- Sys.getenv("CEM_DATABASE_INFO_SCHEMA")

apiPort <- httpuv::randomPort(8000, 8080)
apiUrl <- paste0("http://localhost:", apiPort)

sessionCommunication <- tempfile("CemConnector")
writeLines("", con = sessionCommunication)
print("Starting api session...")
apiSession <- callr::r_session$new()

withr::defer({
  apiSession$kill()
  unlink(jDriverPath)
}, testthat::teardown_env())

apiSession$call(serverStart,
                args = list(pipe = sessionCommunication,
                            apiPort = apiPort,
                            server = Sys.getenv("CEM_DATABASE_SERVER"),
                            user = Sys.getenv("CEM_DATABASE_USER"),
                            password = Sys.getenv("CEM_DATABASE_PASSWORD"),
                            port = Sys.getenv("CEM_DATABASE_PORT"),
                            dbms = Sys.getenv("CEM_DATABASE_DBMS"),
                            extraSettings = Sys.getenv("CEM_DATABASE_EXTRA_SETTINGS"),
                            pathToDriver = jDriverPath,
                            cemSchema = cemTestSchema,
                            vocabularySchema = vocabularySchema,
                            sourceSchema = sourceInfoSchema))

apiSessionReady <- function() {
  if (apiSession$is_alive()) {
    input <- readLines(sessionCommunication)
    return(any(grep("API LOADED", input) == 1))
  }
  # If the session is dead, stop
  stop("Api session failed to load")
}

# poll status until failure or load
while(!apiSessionReady()) {
  Sys.sleep(0.3) # Allow time for process to start, needs to connect to database...
}

print("Session started")
