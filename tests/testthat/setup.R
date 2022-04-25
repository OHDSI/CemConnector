# Run extra/test/realInstanceTest.R For testing against a real CEM instance
apiUrl <- getOption("CemConnector.useHostedUrl")
connectionDetails <- getOption("CemConnectionDetails")
cemTestSchema <- getOption("cemTestSchema")
vocabularySchema <- getOption("cemVocabularySchema")
sourceInfoSchema <- getOption("cemSourceInfoSchema")
useTestPlumber <- FALSE

if (is.null(apiUrl) | !("connectionDetails" %in% class(connectionDetails))) {
  # Load API in separate process
  serverStart <- function(pipe, apiPort, cemDatabaseSchema, vocabularySchema, sourceDatabaseSchema, ...) {
    library(CemConnector)
    connectionDetails <- DatabaseConnector::createConnectionDetails(...)

    tryCatch(
      {
        api <- loadApi(connectionDetails,
                       cemDatabaseSchema = cemDatabaseSchema,
                       vocabularyDatabaseSchema = vocabularySchema,
                       sourceDatabaseSchema = sourceDatabaseSchema
        )
        api$setDocs(FALSE)
        writeLines("API LOADED", con = pipe)
        api$run(port = apiPort)
      },
      error = function(err) {
        writeLines("API FAILED", con = pipe)
        writeLines(err, con = pipe)
      }
    )
  }

  sqlidb <- tempfile(fileext = ".sqlite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = sqlidb)
  .loadCemTestFixtures(connectionDetails)

  withr::defer(
    {
      unlink(sqlidb)
    },
    testthat::teardown_env()
  )

  cemTestSchema <- "main"
  vocabularySchema <- "main"
  sourceInfoSchema <- "main"

  apiPort <- httpuv::randomPort(8000, 8080)
  apiUrl <- paste0("http://localhost:", apiPort)

  sessionCommunication <- tempfile()
  writeLines("", con = sessionCommunication)
  print("Starting api session...")

  stdOut <- tempfile()
  errorOut <- tempfile()

  apiSession <- callr::r_bg(serverStart,
    stdout = stdOut,
    stderr = errorOut,
    package = TRUE,
    args = list(
      pipe = sessionCommunication,
      apiPort = apiPort,
      dbms = "sqlite",
      server = sqlidb,
      cemDatabaseSchema = cemTestSchema,
      vocabularySchema = vocabularySchema,
      sourceDatabaseSchema = sourceInfoSchema
    )
  )

  withr::defer(
    {
      apiSession$kill()
      unlink(sessionCommunication)
      unlink(stdOut)
      unlink(errorOut)
    },
    testthat::teardown_env()
  )


  apiSessionReady <- function() {
    if (apiSession$is_alive()) {
      input <- readLines(sessionCommunication)
      failed <- any(grep("API FAILED", input) == 1)
      loaded <- any(grep("API LOADED", input) == 1)

      if (failed) {
        errorLines <- readLines(errorOut)
        studLines <- readLines(stdOut)
        stop("Failed to load API. Error in configuration?\n", errorLines, studLines)
      }

      return(loaded)
    }
    # If the session is dead, stop
    errorLines <- readLines(errorOut)
    studLines <- readLines(stdOut)
    stop(paste("Api session failed to start\n", errorLines, studLines))
  }

  tryCatch(
    {
      # poll status until failure or load
      while (!apiSessionReady()) {
        Sys.sleep(0.01) # Allow time for process to start, needs to connect to database...
      }
      useTestPlumber <- TRUE
      print("Session started")
    },
    error = function(err) {
      message("Failed to load API will skip web request tests")
      print(err)
    }
  )
} else {
  useTestPlumber <- TRUE
  message(paste("Using live web backend at", apiUrl))
}
