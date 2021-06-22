# Implementations of database connection should function in the same way
genericTests <- function(connClass, classes) {
  conn <- connClass$new(Eunomia::getEunomiaConnectionDetails())
  expect_class(conn, classes)

  on.exit({
    conn$finalize()
  }, add = TRUE)

  expect_class(conn, "ConnectionHandler")
  expect_true(conn$isActive)
  expect_true(DBI::dbIsValid(dbObj = conn$con))

  data <- conn$queryDb("SELECT count(*) AS cnt FROM main.person")

  expect_data_frame(data)
  expect_equal(data$cnt, 2694)

  conn$closeConnection()
  expect_false(conn$isActive)
  expect_false(DBI::dbIsValid(dbObj = conn$con))
  conn$initConnection()
  expect_true(conn$isActive)

  expect_warning(conn$initConnection(), "Closing existing connection")
  conn$closeConnection()
}

test_that("Database Connector Class works", {
  genericTests(ConnectionHandler, c("ConnectionHandler"))
})

test_that("Pooled connector Class works", {
  genericTests(PooledConnectionHandler, c("PooledConnectionHandler", "ConnectionHandler"))
})