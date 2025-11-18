test_that("validate_codeminer_db_path errors when env var not set", {
  # Temporarily unset CODEMINER_DB_PATH
  withr::local_envvar(CODEMINER_DB_PATH = NA)

  # Should error about missing env var
  expect_error(
    codeminer.api:::validate_codeminer_db_path(),
    "CODEMINER_DB_PATH.*not set"
  )
})

test_that("validate_codeminer_db_path errors when file doesn't exist", {
  # Set to non-existent file
  withr::local_envvar(CODEMINER_DB_PATH = "/nonexistent/path/to/db.duckdb")

  # Should error about missing file
  expect_error(
    codeminer.api:::validate_codeminer_db_path(),
    "Database file not found"
  )
})

test_that("validate_codeminer_db_path returns path when valid", {
  # Uses the dummy database from setup.R
  # Should return the path invisibly
  result <- codeminer.api:::validate_codeminer_db_path()
  expect_equal(result, Sys.getenv("CODEMINER_DB_PATH"))
})

test_that("run_codeminer_api_foreground validates CODEMINER_DB_PATH", {
  # Temporarily unset CODEMINER_DB_PATH
  withr::local_envvar(CODEMINER_DB_PATH = NA)

  # Should error about missing env var
  expect_error(
    codeminer.api:::run_codeminer_api_foreground(quiet = TRUE),
    "CODEMINER_DB_PATH.*not set"
  )
})

test_that("run_codeminer_api_foreground validates database file exists", {
  # Set to non-existent file
  withr::local_envvar(CODEMINER_DB_PATH = "/nonexistent/path/to/db.duckdb")

  # Should error about missing file
  expect_error(
    codeminer.api:::run_codeminer_api_foreground(quiet = TRUE),
    "Database file not found"
  )
})

test_that("run_codeminer_api_foreground creates router and calls pr_run", {
  # Track if pr_run was called
  pr_run_called <- FALSE
  pr_run_args <- NULL

  local_mocked_bindings(
    create_codeminer_api = function() plumber::pr(),
    .package = "codeminer.api"
  )

  local_mocked_bindings(
    pr_run = function(pr, ...) {
      pr_run_called <<- TRUE
      pr_run_args <<- list(...)
      NULL
    },
    .package = "plumber"
  )

  # Call the internal function
  codeminer.api:::run_codeminer_api_foreground(host = "127.0.0.1", port = 9000, quiet = TRUE)

  # Verify pr_run was called
  expect_true(pr_run_called)
  expect_equal(pr_run_args$host, "127.0.0.1")
  expect_equal(pr_run_args$port, 9000)
  expect_equal(pr_run_args$quiet, TRUE)
})

test_that("run_codeminer_api_foreground disables docs when docs = FALSE", {
  # Track if pr_set_docs was called
  pr_set_docs_called <- FALSE
  pr_set_docs_args <- NULL

  local_mocked_bindings(
    create_codeminer_api = function() plumber::pr(),
    .package = "codeminer.api"
  )

  local_mocked_bindings(
    pr_set_docs = function(pr, value) {
      pr_set_docs_called <<- TRUE
      pr_set_docs_args <<- value
      pr
    },
    pr_run = function(...) NULL,
    .package = "plumber"
  )

  # Call with docs disabled
  codeminer.api:::run_codeminer_api_foreground(docs = FALSE, quiet = TRUE)

  # Verify pr_set_docs was called with FALSE
  expect_true(pr_set_docs_called)
  expect_equal(pr_set_docs_args, FALSE)
})

test_that("run_codeminer_api calls run_codeminer_api_foreground in foreground mode", {
  # Track if the internal function was called
  foreground_called <- FALSE
  foreground_args <- NULL

  local_mocked_bindings(
    run_codeminer_api_foreground = function(...) {
      foreground_called <<- TRUE
      foreground_args <<- list(...)
      NULL
    },
    .package = "codeminer.api"
  )

  # Run in foreground
  run_codeminer_api(host = "127.0.0.1", port = 9000, docs = FALSE, quiet = TRUE)

  # Verify internal function was called
  expect_true(foreground_called)
  expect_equal(foreground_args$host, "127.0.0.1")
  expect_equal(foreground_args$port, 9000)
  expect_equal(foreground_args$docs, FALSE)
  expect_equal(foreground_args$quiet, TRUE)
})

test_that("run_codeminer_api calls callr::r_bg in background mode", {
  # Track if r_bg was called
  r_bg_called <- FALSE
  r_bg_args <- NULL
  r_bg_env <- NULL
  mock_bg_process <- list(is_alive = function() TRUE)

  local_mocked_bindings(
    r_bg = function(func, args, env, ...) {
      r_bg_called <<- TRUE
      r_bg_args <<- args
      r_bg_env <<- env
      mock_bg_process
    },
    .package = "callr"
  )

  # Run in background mode
  result <- run_codeminer_api(
    host = "0.0.0.0",
    port = 7777,
    background = TRUE,
    docs = FALSE,
    quiet = TRUE
  )

  # Verify r_bg was called
  expect_true(r_bg_called)

  # Verify arguments passed to background process
  expect_equal(r_bg_args$host, "0.0.0.0")
  expect_equal(r_bg_args$port, 7777)
  expect_equal(r_bg_args$docs, FALSE)
  expect_equal(r_bg_args$quiet, TRUE)

  # Verify env vars are passed via env argument
  expect_true("CODEMINER_DB_PATH" %in% names(r_bg_env))

  # Verify result is the background process
  expect_equal(result, mock_bg_process)
})
