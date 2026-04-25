test_that("capture_cm_condition() extracts messages correctly", {
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- character()

  # Fake CodeMiner warning with cli_message
  cnd <- structure(
    list(cli_message = "interpolated"),
    class = c("codeminer_warning", "warning", "condition")
  )

  capture_cm_condition("warnings", warn_env)(cnd)

  expect_identical(warn_env$warnings, list(list("-" = "interpolated")))
})

test_that("capture_cm_condition() muffles warnings when restart is available", {
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- character()

  triggered <- FALSE

  withCallingHandlers(
    {
      # This warning SHOULD NOT propagate if muffleWarning is called
      warning("base test warning")
      triggered <<- TRUE
    },
    warning = capture_cm_condition("warnings", warn_env),
    muffleWarning = function() {}
  )

  # Condition captured
  expect_identical(warn_env$warnings, list(c("-" = "base test warning")))

  # The underlying warning was silenced
  expect_false(triggered)
})

test_that("set_missing_names adds names to unnamed vector", {
  expect_equal(
    set_missing_names(c("foo", "bar")),
    c("-" = "foo", "-" = "bar")
  )
})

test_that("set_missing_names fills missing/empty names", {
  x <- c(a = "foo", "bar", b = "baz", NA, "qux")
  names(x)[4] <- "" # one empty name
  names(x)[5] <- NA # one NA name
  expect_equal(
    set_missing_names(x),
    c(a = "foo", "-" = "bar", b = "baz", "-" = NA, "-" = "qux")
  )
})

test_that("set_missing_names uses custom missing name", {
  expect_equal(
    set_missing_names(c("foo", "bar"), missing = "MISSING"),
    c(MISSING = "foo", MISSING = "bar")
  )
})

test_that("set_missing_names leaves fully named vector unchanged", {
  x <- c(a = "foo", b = "bar")
  expect_identical(set_missing_names(x), x)
})

test_that("codeminer_handle captures codeminer_message conditions", {
  expr <- function() {
    cli::cli_inform(
      c("i" = "Hello", "i" = "World"),
      class = "codeminer_message",
      cli_message = c("i" = "Hello", "i" = "World")
    )
    "OK"
  }

  res <- new.env()
  output <- codeminer_handle(expr(), res)

  expect_equal(output$result, "OK")
  expect_equal(
    output$messages[[2]], # skip N-messages entry
    list("i" = "Hello", "i" = "World")
  )
})

test_that("codeminer_handle captures codeminer_warning conditions", {
  expr <- function() {
    cli::cli_warn(
      c("!" = "W1", "!" = "W2"),
      class = "codeminer_warning",
      cli_message = c("!" = "W1", "!" = "W2")
    )
    "OK"
  }

  res <- new.env()
  output <- codeminer_handle(expr(), res)

  expect_equal(output$result, "OK")
  expect_equal(
    output$warnings[[2]],
    list("!" = "W1", "!" = "W2")
  )
})

test_that("codeminer_handle captures codeminer_error", {
  expr <- function() {
    cli::cli_abort(
      c("x" = "Bad!", ">" = "Oops"),
      class = "codeminer_error",
      cli_message = c("x" = "Bad!", ">" = "Oops")
    )
  }

  res <- new.env()
  output <- codeminer_handle(expr(), res)

  expect_null(output$result)
  expect_equal(output$error$error_type, "codeminer_error")

  expect_equal(
    output$error$error_message,
    list("x" = "Bad!", ">" = "Oops")
  )
})
