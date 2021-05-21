# Implementations of database connection should function in the same way
genericTests <- function(conn) {
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
  conn <- ConnectionHandler$new(Eunomia::getEunomiaConnectionDetails())
  expect_class(conn, c("ConnectionHandler"))
  genericTests(conn)
})

test_that("Pooled connector Class works", {
  conn <- PooledConnectionHandler$new(Eunomia::getEunomiaConnectionDetails())
  expect_class(conn, c("PooledConnectionHandler", "ConnectionHandler"))
  genericTests(conn)
})