###
# This script creates the test database schema, in the case of github actions this is a postgres docker instance.
# The data is a subet of the CEM for the contained unit tests.
###
createTestSchema <- function() {
  connectionDetails <- DatabaseConnector::createConnectionDetails(server = Sys.getenv("TEST_CEM_DATABASE_SERVER"),
                                                                user = Sys.getenv("TEST_CEM_DATABASE_USER"),
                                                                password = Sys.getenv("TEST_CEM_DATABASE_PASSWORD"),
                                                                port = Sys.getenv("TEST_CEM_DATABASE_PORT"),
                                                                dbms = Sys.getenv("TEST_CEM_DATABASE_DBMS"),
                                                                extraSettings = Sys.getenv("TEST_CEM_DATABASE_EXTRA_SETTINGS"))

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit({ DatabaseConnector::disconnect(connection) }, add = TRUE)

  # Read and run test data sql insert
  sql <- SqlRender::readSql(file.path("extra", "test", "testDataDdl.sql"))

  cemTestSchema <- Sys.getenv("TEST_CEM_DATABASE_SCHEMA")
  vocabularySchema <- Sys.getenv("TEST_CEM_DATABASE_VOCAB_SCHEMA")
  sourceInfoSchema <- Sys.getenv("TEST_CEM_DATABASE_INFO_SCHEMA")

  sql <- SqlRender::render(sql,
                           cem_schema = cemTestSchema,
                           cem_info_schema = vocabularySchema,
                           vocabulary_schema = sourceInfoSchema)

  DatabaseConnector::executeSql(connection, sql)

  for (tableName in c("CONCEPT", "CONCEPT_ANCESTOR", "MATRIX_SUMMARY", "CEM_UNIFIED")) {
    csvName <- paste0(tableName, ".csv")
    data <- read.csv(file.path("extra", "test", csvName))
    DatabaseConnector::insertTable(connection = connection, tableName = tableName, databaseSchema = cemTestSchema, data = data)
  }
}

createTestSchema()
