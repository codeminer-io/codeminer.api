# Set up test environment
# Only create dummy database if codeminer is available
if (requireNamespace("codeminer", quietly = TRUE)) {
  # Create a dummy database file for tests
  dummy_db <- tempfile(fileext = ".duckdb")
  codeminer::create_dummy_database(dummy_db, .envir = .GlobalEnv)
  
  # Clean up on exit
  withr::defer(
    {
      if (file.exists(dummy_db)) {
        unlink(dummy_db)
      }
    },
    teardown_env()
  )
} else {
  # If codeminer not available, set a basic path
  # (tests requiring codeminer will skip anyway)
  dummy_db <- tempfile(fileext = ".duckdb")
  file.create(dummy_db)
  Sys.setenv(CODEMINER_DB_PATH = dummy_db)
  
  withr::defer(
    {
      unlink(dummy_db)
    },
    teardown_env()
  )
}
