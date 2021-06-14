# Load API in separate process
serverStart <- function(apiPort, cemSchema, vocabularySchema, sourceSchema, ...) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(...)
  api <- CemConnector::loadApi(connectionDetails,
                               cemSchema = cemSchema,
                               vocabularySchema = vocabularySchema,
                               sourceSchema = sourceSchema)
  writeLines("*** API LOADED ***", con = stdout())
  api$run(port = apiPort)
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

print("Starting api session...")
apiSession <- callr::r_session$new()
apiSession$call(serverStart,
                args = list(apiPort = apiPort,
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
}, testthat::teardown_env())

Sys.sleep(4) # Allow time for process to start, needs to connect to database...

print("Session started")
