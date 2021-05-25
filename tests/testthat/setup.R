keyringService <- Sys.getenv("CEM_KEYRING_SERVICE")
dbUser <- Sys.getenv("CEM_DATABASE_USER")

connectionDetails <- DatabaseConnector::createConnectionDetails(server = Sys.getenv("CEM_DATABASE_SERVER"),
                                                                user = dbUser,
                                                                password = keyring::key_get(keyringService, username = dbUser),
                                                                port = Sys.getenv("CEM_DATABASE_PORT"),
                                                                dbms = Sys.getenv("CEM_DATABASE_DBMS"),
                                                                extraSettings = Sys.getenv("CEM_DATABASE_EXTRA_SETTINGS"))

cemTestSchema <- Sys.getenv("CEM_DATABASE_SCHEMA")
vocabularySchema <- Sys.getenv("CEM_DATABASE_VOCAB_SCHEMA")
sourceInfoSchema <- Sys.getenv("CEM_DATABASE_INFO_SCHEMA")