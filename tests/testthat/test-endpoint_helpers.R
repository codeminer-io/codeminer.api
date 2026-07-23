test_that("capture_cm_condition() captures a structured object", {
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- list()

  # Fake CodeMiner warning with cli_message (bare warning -> empty type)
  cnd <- structure(
    list(cli_message = "interpolated"),
    class = c("codeminer_warning", "rlang_warning", "warning", "condition")
  )

  capture_cm_condition("warnings", warn_env)(cnd)

  expect_identical(
    warn_env$warnings,
    list(list(type = character(0), message = list("-" = "interpolated")))
  )
})

test_that("capture_cm_condition() preserves subclass and data fields", {
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- list()

  # Mirrors codeminer::missing_codes_warning(): a subclassed warning carrying
  # structured data fields.
  cnd <- structure(
    list(
      cli_message = c("!" = "Codes not found", "*" = "ZZ9"),
      missing_codes = c("ZZ9", "YY8"),
      table_type = "lookup",
      table_meta = list(name = "x")
    ),
    class = c(
      "codeminer_missing_codes",
      "codeminer_warning",
      "rlang_warning",
      "warning",
      "condition"
    )
  )

  capture_cm_condition("warnings", warn_env)(cnd)

  captured <- warn_env$warnings[[1]]
  # type drops the base classes, mirroring error_type
  expect_identical(captured$type, "codeminer_missing_codes")
  expect_identical(captured$message, list("!" = "Codes not found", "*" = "ZZ9"))
  # data fields carried through
  expect_identical(captured$missing_codes, c("ZZ9", "YY8"))
  expect_identical(captured$table_type, "lookup")
  expect_identical(captured$table_meta, list(name = "x"))
})

test_that("capture_cm_condition() muffles warnings when restart is available", {
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- list()

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

  # Condition captured (base R warning -> "simpleWarning" survives the setdiff)
  expect_identical(
    warn_env$warnings,
    list(list(
      type = "simpleWarning",
      message = list("-" = "base test warning")
    ))
  )

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

  output <- codeminer_handle(expr())

  expect_equal(output$result, "OK")
  # skip the N-messages count entry; message text under $message
  expect_equal(output$messages[[2]]$type, character(0))
  expect_equal(
    output$messages[[2]]$message,
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

  output <- codeminer_handle(expr())

  expect_equal(output$result, "OK")
  expect_equal(output$warnings[[2]]$type, character(0))
  expect_equal(
    output$warnings[[2]]$message,
    list("!" = "W1", "!" = "W2")
  )
})

test_that("codeminer_handle captures a subclassed warning with data fields", {
  expr <- function() {
    cli::cli_warn(
      c("!" = "Codes not found"),
      class = c("codeminer_missing_codes", "codeminer_warning"),
      cli_message = c("!" = "Codes not found"),
      missing_codes = c("ZZ9", "YY8"),
      table_type = "lookup"
    )
    "OK"
  }

  output <- codeminer_handle(expr())

  captured <- output$warnings[[2]]
  expect_equal(captured$type, "codeminer_missing_codes")
  expect_equal(captured$message, list("!" = "Codes not found"))
  expect_equal(captured$missing_codes, c("ZZ9", "YY8"))
  expect_equal(captured$table_type, "lookup")
})

test_that("codeminer_handle ignores base R warnings and messages", {
  expr <- function() {
    warning("a base R warning")
    message("a base R message")
    "OK"
  }

  output <- suppressWarnings(suppressMessages(codeminer_handle(expr())))

  expect_equal(output$result, "OK")
  # only the count element remains - base conditions are not captured
  expect_equal(output$warnings, list("Warnings: 0"))
  expect_equal(output$messages, list("Messages: 0"))
})

test_that("codeminer_handle formats an unexpected error with status 500", {
  expr <- function() {
    stop("boom")
  }

  output <- suppressWarnings(codeminer_handle(expr()))

  expect_null(output$result)
  expect_equal(output$error$status, 500)
  expect_equal(output$error$error_type, "Backend Error")
  expect_equal(output$error$error_message, list(x = "boom"))
})

test_that("codeminer_handle captures codeminer_error", {
  expr <- function() {
    cli::cli_abort(
      c("x" = "Bad!", ">" = "Oops"),
      class = "codeminer_error",
      cli_message = c("x" = "Bad!", ">" = "Oops")
    )
  }

  output <- codeminer_handle(expr())

  expect_null(output$result)
  expect_equal(output$error$status, 422)
  expect_equal(output$error$error_type, "codeminer_error")

  expect_equal(
    output$error$error_message,
    list("x" = "Bad!", ">" = "Oops")
  )
})

test_that("codeminer_handle preserves the codeminer-specific class chain", {
  expr <- function() {
    cli::cli_abort(
      c("x" = "Too big!"),
      class = c("codeminer_max_tree_codes_exceeded", "codeminer_error"),
      cli_message = c("x" = "Too big!")
    )
  }

  output <- codeminer_handle(expr())

  # Full codeminer chain sent; base R condition classes dropped (cli_abort
  # re-adds them client-side).
  expect_equal(
    output$error$error_type,
    c("codeminer_max_tree_codes_exceeded", "codeminer_error")
  )
})
