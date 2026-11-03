# Against a live API with a SNOMED CT database. Set CODEMINER_LIVE_API_URL to
# run, e.g. "http://localhost:8001"; skipped otherwise so CI and the dummy
# database tests are unaffected.

test_that("codeminer.col_filters applies to every query against a live API", {
  live_url <- Sys.getenv("CODEMINER_LIVE_API_URL")
  skip_if(!nzchar(live_url), "CODEMINER_LIVE_API_URL not set")
  skip_if_offline()

  withr::local_options(
    codeminer.api.url = live_url,
    codeminer.code_type = NULL,
    codeminer.col_filters = NULL
  )
  all_edges <- list(relationship = list("SNOMED CT" = NA))
  # Warfarin (substance); ingredient relationship types
  warfarin <- function(...) {
    HAS_ATTRIBUTES(
      "372756006",
      type = "SNOMED CT",
      relationship_types = c("127489000", "762949000", "10362801000001104"),
      ...
    ) |>
      suppressMessages() |>
      suppressWarnings()
  }

  default <- warfarin()
  explicit <- warfarin(col_filters = all_edges)
  withr::with_options(
    list(codeminer.col_filters = all_edges),
    via_option <- warfarin()
  )
  withr::with_options(
    list(codeminer.col_filters = all_edges),
    explicit_default <- warfarin(col_filters = "default")
  )

  # Row order varies between calls, so compare the sets of codes
  n <- function(x) length(unique(x$code))
  expect_setequal(via_option$code, explicit$code)
  expect_identical(nrow(via_option), nrow(explicit))
  expect_gt(n(explicit), 5 * n(default))
  expect_setequal(explicit_default$code, default$code)
  expect_identical(nrow(explicit_default), nrow(default))
})
