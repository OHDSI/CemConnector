# Implementations of database connection should function in the same way
genericTests <- function(conn) {
  expect_class(conn, "Connection")
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
  conn <- Connection$new(Eunomia::getEunomiaConnectionDetails())
  genericTests(conn)
})

test_that("Pooled connector Class works", {
  conn <- PooledConnection$new(Eunomia::getEunomiaConnectionDetails())
  expect_class(conn, "PooledConnection")
  genericTests(conn)
})