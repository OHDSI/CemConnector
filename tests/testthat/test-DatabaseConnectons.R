# Implementations of database connection should function in the same way
genericTests <- function(connClass, classes, connectionClass) {
  conn <- connClass$new(connectionDetails)
  expect_class(conn, classes)

  on.exit(
    {
      conn$finalize()
    },
    add = TRUE
  )

  expect_class(conn, "ConnectionHandler")
  expect_true(conn$isActive)
  expect_true(DBI::dbIsValid(dbObj = conn$con))

  withr::with_envvar(new = c("LIMIT_ROW_COUNT" = 1), {
    data <- conn$queryDb("SELECT * FROM main.concept;")
    expect_equal(nrow(data), 1)
  })

  withr::with_envvar(new = c("LIMIT_ROW_COUNT" = 5), {
    data <- conn$queryDb("SELECT * FROM main.concept")
    expect_equal(nrow(data), 5)
  })

  data <- conn$queryDb("SELECT count(*) AS cnt_test FROM main.concept;")

  expect_data_frame(data)
  expect_equal(data$cntTest, 5167)

  data2 <- conn$queryDb("SELECT count(*) AS cnt_test FROM main.concept;", snakeCaseToCamelCase = FALSE)

  expect_data_frame(data2)
  expect_equal(data2$CNT_TEST, 5167)

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
