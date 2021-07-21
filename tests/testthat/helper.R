.loadCemTestFixtures <- function(connectionDetails) {
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  files <- c(system.file("test", "MATRIX_SUMMARY.csv", package = "CemConnector"),
             system.file("test", "CEM_UNIFIED.csv", package = "CemConnector"),
             system.file("test", "CONCEPT_ANCESTOR.csv", package = "CemConnector"),
             system.file("test", "CONCEPT.csv", package = "CemConnector"),
             system.file("test", "SOURCE.csv", package = "CemConnector"))

  for (tbl in files) {
    data <- read.csv(tbl)
    tableName <- gsub(".csv", "", basename(tbl))
    DatabaseConnector::insertTable(connection = connection,
                                   tableName = tableName,
                                   data = data)
  }
}

