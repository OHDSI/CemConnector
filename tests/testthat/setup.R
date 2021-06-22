apiUrl <- getOption("CemConnector.useHostedUrl")
connectionDetails <- getOption("CemConnectionDetails")
cemTestSchema <- getOption("cemTestSchema")
vocabularySchema <- getOption("cemVocabularySchema")
sourceInfoSchema <- getOption("cemSourceInfoSchema")

if (is.null(apiUrl) | !("connectionDetails" %in% class(connectionDetails))) {
  # Load API in separate process
  serverStart <- function(pipe, apiPort, cemSchema, vocabularySchema, sourceSchema, ...) {
    connectionDetails <- DatabaseConnector::createConnectionDetails(...)

    tryCatch({
      api <- CemConnector::loadApi(connectionDetails,
                                   cemSchema = cemSchema,
                                   vocabularySchema = vocabularySchema,
                                   sourceSchema = sourceSchema)
      api$setDocs(FALSE)
      writeLines("API LOADED", con = pipe)
      api$run(port = apiPort, host = "0.0.0.0")
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

  sessionCommunication <- paste0("test_pipe_", apiPort)
  writeLines("", con = sessionCommunication)
  print("Starting api session...")

  apiSession <- callr::r_bg(serverStart,
                            stdout = "plumberOut.txt",
                            stderr = "plumberError.txt",
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

  withr::defer({
    apiSession$kill()
    unlink(jDriverPath)
    unlink(sessionCommunication)
  }, testthat::teardown_env())


  apiSessionReady <- function() {
    if (apiSession$is_alive()) {
      input <- readLines(sessionCommunication)
      failed <- any(grep("API FAILED", input) == 1)
      loaded <- any(grep("API LOADED", input) == 1)

      if (failed) {
        errorLines <- readLines("plumberError.txt")
        studOut <- readLines("plumberOut.txt")
        stop("Failed to load API. Error in configuration?\n", errorLines, studOut)
      }

      return(loaded)
    }
    # If the session is dead, stop
    errorLines <- readLines("plumberError.txt")
    studOut <- readLines("plumberOut.txt")
    stop(paste("Api session failed to start\n", errorLines, studOut))
  }

  # poll status until failure or load
  while (!apiSessionReady()) {
    Sys.sleep(0.1) # Allow time for process to start, needs to connect to database...
  }

  print("Session started")
} else {
  message(paste("Using live web backend at", apiUrl))
}
