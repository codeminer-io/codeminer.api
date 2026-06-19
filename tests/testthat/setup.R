# Set up test environment
# Create a dummy database file for tests
dummy_db <- withr::local_tempfile(
  fileext = ".duckdb",
  .local_envir = teardown_env()
)
codeminer::create_dummy_database(dummy_db, .envir = .GlobalEnv)

# The dummy database's relationship tables are all purely hierarchical, so the
# type-dimension functions (ATTRIBUTES_FOR / RELATIONSHIP_TYPES*) are not
# applicable to them. Add a small multi-type relationship table whose type
# values are themselves codes in the lookup (so descriptions resolve), used by
# the type-dimension endpoint/client tests.
codeminer::add_lookup_table(
  data.frame(
    code = c("disorder", "finding", "site", "T_isa", "T_attr"),
    description = c(
      "A disorder",
      "A finding",
      "A body site",
      "Is a",
      "Finding site"
    )
  ),
  codeminer::lookup_metadata("api_multi")
)
codeminer::add_relationship_table(
  data.frame(
    from = c("disorder", "disorder"),
    to = c("finding", "site"),
    type = c("T_isa", "T_attr")
  ),
  codeminer::relationship_metadata(
    "api_multi",
    type_col = "type",
    child_parent_relationship_code = "T_isa"
  )
)
