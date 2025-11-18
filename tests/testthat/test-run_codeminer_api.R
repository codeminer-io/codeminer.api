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
