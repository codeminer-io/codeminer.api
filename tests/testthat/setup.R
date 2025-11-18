# Set up test environment
# Create a dummy database file for tests
dummy_db <- withr::local_tempfile(
  fileext = ".duckdb",
  .local_envir = teardown_env()
)
codeminer::create_dummy_database(dummy_db, .envir = .GlobalEnv)
