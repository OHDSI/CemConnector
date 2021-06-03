connectionDetails <- DatabaseConnector::createConnectionDetails(server = Sys.getenv("TEST_CEM_DATABASE_SERVER"),
                                                                user = Sys.getenv("TEST_CEM_DATABASE_USER"),
                                                                password = Sys.getenv("TEST_CEM_DATABASE_PASSWORD"),
                                                                port = Sys.getenv("TEST_CEM_DATABASE_PORT"),
                                                                dbms = Sys.getenv("TEST_CEM_DATABASE_DBMS"),
                                                                extraSettings = Sys.getenv("TEST_CEM_DATABASE_EXTRA_SETTINGS"))

cemTestSchema <- Sys.getenv("TEST_CEM_DATABASE_SCHEMA")
vocabularySchema <- Sys.getenv("TEST_CEM_DATABASE_VOCAB_SCHEMA")
sourceInfoSchema <- Sys.getenv("TEST_CEM_DATABASE_INFO_SCHEMA")

apiPort <- httpuv::randomPort(8000, 8080)
apiUrl <- paste0("http://localhost:", apiPort)
# Load API in separate process
serverStart <- function(apiPort, ...) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(...)
  api <- CEMConnector::loadApi(connectionDetails,
                               cemSchema = Sys.getenv("TEST_CEM_DATABASE_SCHEMA"),
                               vocabularySchema = Sys.getenv("TEST_CEM_DATABASE_VOCAB_SCHEMA"),
                               sourceSchema = Sys.getenv("TEST_CEM_DATABASE_INFO_SCHEMA"))
  writeLines("*** API LOADED ***", con = stdout())
  api$run(port = apiPort)
}

apiSession <- callr::r_session$new()

print("Starting api session...")
apiSession$call(serverStart,
                args = list(apiPort = apiPort,
                            server = Sys.getenv("TEST_CEM_DATABASE_SERVER"),
                            user = Sys.getenv("TEST_CEM_DATABASE_USER"),
                            password = Sys.getenv("TEST_CEM_DATABASE_PASSWORD"),
                            port = Sys.getenv("TEST_CEM_DATABASE_PORT"),
                            dbms = Sys.getenv("TEST_CEM_DATABASE_DBMS"),
                            extraSettings = Sys.getenv("TEST_CEM_DATABASE_EXTRA_SETTINGS")))
withr::defer({
  apiSession$kill()
}, testthat::teardown_env())


Sys.sleep(4) # Allow time for process to start, needs to connect to database

print("Session started")
