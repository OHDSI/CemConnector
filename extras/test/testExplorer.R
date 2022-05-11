
connectionDetails <- DatabaseConnector::createConnectionDetails(server = Sys.getenv("LIVE_CEM_DATABASE_SERVER"),
                                                                user = dbUser,
                                                                password = keyring::key_get(keyringService, username = dbUser),
                                                                port = Sys.getenv("LIVE_CEM_DATABASE_PORT"),
                                                                dbms = Sys.getenv("LIVE_CEM_DATABASE_DBMS"),
                                                                extraSettings = Sys.getenv("LIVE_CEM_DATABASE_EXTRA_SETTINGS"))

cemTestSchema <- Sys.getenv("LIVE_CEM_DATABASE_SCHEMA")
vocabularyDatabaseSchema <- Sys.getenv("LIVE_CEM_DATABASE_VOCAB_SCHEMA")
sourceInfoSchema <- Sys.getenv("LIVE_CEM_DATABASE_INFO_SCHEMA")

backend <- CemConnector::CemDatabaseBackend$new(connectionDetails = connectionDetails, cemDatabaseSchema = cemTestSchema, vocabularyDatabaseSchema = vocabularyDatabaseSchema, sourceDatabaseSchema = sourceInfoSchema)

CemConnector::launchEvidenceExplorer(backend)
