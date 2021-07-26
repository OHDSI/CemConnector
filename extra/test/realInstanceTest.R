####
# Run this code to create a test instance of CEM that runs in a background process against real CEM data
# When this script runs first the testthat tests will not run against the test instance and will use the real data.
# These envionment variables must be set:
# LIVE_CEM_DATABASE_SERVER
# LIVE_CEM_DATABASE_PORT
# LIVE_CEM_DATABASE_EXTRA_SETTINGS
# LIVE_CEM_KEYRING_SERVICE
# LIVE_CEM_DATABASE_USER
# LIVE_CEM_DATABASE_SCHEMA
# LIVE_CEM_DATABASE_VOCAB_SCHEMA
# LIVE_CEM_DATABASE_INFO_SCHEMA
####
keyringService <- Sys.getenv("LIVE_CEM_KEYRING_SERVICE")
dbUser <- Sys.getenv("LIVE_CEM_DATABASE_USER")

connectionDetails <- DatabaseConnector::createConnectionDetails(server = Sys.getenv("LIVE_CEM_DATABASE_SERVER"),
                                                                user = dbUser,
                                                                password = keyring::key_get(keyringService, username = dbUser),
                                                                port = Sys.getenv("LIVE_CEM_DATABASE_PORT"),
                                                                dbms = Sys.getenv("LIVE_CEM_DATABASE_DBMS"),
                                                                extraSettings = Sys.getenv("LIVE_CEM_DATABASE_EXTRA_SETTINGS"))

cemTestSchema <- Sys.getenv("LIVE_CEM_DATABASE_SCHEMA")
vocabularySchema <- Sys.getenv("LIVE_CEM_DATABASE_VOCAB_SCHEMA")
sourceInfoSchema <- Sys.getenv("LIVE_CEM_DATABASE_INFO_SCHEMA")

apiPort <- httpuv::randomPort(8000, 8080)
apiUrl <- paste0("http://localhost:", apiPort)
# Load API in separate process
serverStart <- function(apiPort, ...) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(...)
  api <- CemConnector::loadApi(connectionDetails,
                               cemSchema = Sys.getenv("LIVE_CEM_DATABASE_SCHEMA"),
                               vocabularySchema = Sys.getenv("LIVE_CEM_DATABASE_VOCAB_SCHEMA"),
                               sourceSchema = Sys.getenv("LIVE_CEM_DATABASE_INFO_SCHEMA"))
  api$setDocs(FALSE)
  writeLines("API LOADED", con = stdout())
  api$run(port = apiPort)
}

print("Starting api session...")
apiProcess <- callr::r_bg(serverStart,
                          stdout = "api.txt",
                          stderr = "err.txt",
                          args = list(apiPort = apiPort,
                                      server = Sys.getenv("LIVE_CEM_DATABASE_SERVER"),
                                      user = dbUser,
                                      password = keyring::key_get(keyringService, username = dbUser),
                                      port = Sys.getenv("LIVE_CEM_DATABASE_PORT"),
                                      dbms = Sys.getenv("LIVE_CEM_DATABASE_DBMS"),
                                      extraSettings = Sys.getenv("LIVE_CEM_DATABASE_EXTRA_SETTINGS")))

message("running background process at: ", apiUrl)
message("Swagger at: ", apiUrl, "/__docs__/")


options(
  "CemConnector.useHostedUrl" = apiUrl,
  "CemConnectionDetails" = connectionDetails,
  "cemTestSchema" = cemTestSchema,
  "cemVocabularySchema" = vocabularySchema,
  "cemSourceInfoSchema" = sourceInfoSchema
)
