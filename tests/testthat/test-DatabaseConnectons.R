# Implementations of database connection should function in the same way
genericTests <- function(connClass, classes, connectionClass) {
  on.exit({
    unlink(logfile)
  }, add =TRUE)

  conn <- connClass$new(Eunomia::getEunomiaConnectionDetails())
  expect_class(conn, classes)

  on.exit({
    conn$finalize()
  }, add = TRUE)

  expect_class(conn, "ConnectionHandler")
  expect_true(conn$isActive)
  expect_true(DBI::dbIsValid(dbObj = conn$con))

  data <- conn$queryDb("SELECT count(*) AS cnt_test FROM main.person")

  expect_data_frame(data)
  expect_equal(data$cntTest, 2694)

  data2 <- conn$queryDb("SELECT count(*) AS cnt_test FROM main.person", snakeCaseToCamelCase = FALSE)

  expect_data_frame(data2)
  expect_equal(data2$CNT_TEST, 2694)

  expect_error(conn$queryDb("SELECT 1 * WHERE;"))

  conn$closeConnection()
  expect_false(conn$isActive)
  expect_false(DBI::dbIsValid(dbObj = conn$con))
  conn$initConnection()
  expect_true(conn$isActive)

  expect_warning(conn$initConnection(), "Closing existing connection")
  expect_class(conn$getConnection(), connectionClass)
  conn$closeConnection()
}

test_that("Database Connector Class works", {
  genericTests(ConnectionHandler, c("ConnectionHandler"), "DatabaseConnectorDbiConnection")
})

test_that("Pooled connector Class works", {
  genericTests(PooledConnectionHandler, c("PooledConnectionHandler", "ConnectionHandler"), "Pool")
})