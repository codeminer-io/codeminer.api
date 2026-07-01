# The col_filters request contract: the handler factory forwards the body
# value verbatim to codeminer, which accepts "default", NULL, or a
# table-keyed list (list(lookup/relationship/mapping, keyed by code type or
# "from > to")). These tests pin the pass-through at the factory level.

col_filters_test_handler <- codeminer_handler_factory(function(
  codes = NULL,
  type = NULL,
  col_filters = "default"
) {
  codeminer::CODES(codes, type = type, col_filters = col_filters)
})

mock_req <- function(body_json) {
  list(
    args = list(),
    body = jsonlite::fromJSON(body_json, simplifyVector = TRUE)
  )
}

test_that("a table-keyed col_filters body filters the queried table", {
  req <- mock_req(
    '{
      "codes": "all",
      "type": "api_multi",
      "col_filters": {
        "lookup": {"api_multi": {"code": ["disorder", "finding"]}}
      }
    }'
  )
  res <- new.env()

  response <- col_filters_test_handler(req, res)

  expect_setequal(response$result$code, c("disorder", "finding"))
})

test_that("col_filters entries matching no table warn in the envelope", {
  req <- mock_req(
    '{
      "codes": "all",
      "type": "api_multi",
      "col_filters": {
        "lookup": {"api_mutli": {"code": ["disorder"]}}
      }
    }'
  )
  res <- new.env()

  response <- col_filters_test_handler(req, res)

  # The typo'd key is ignored (defaults apply) and the structured warning is
  # replayed to the client. `result` is the unclassed codelist, so check the
  # code vector rather than nrow().
  expect_length(response$result$code, 5)
  warning_types <- unlist(lapply(response$warnings[-1], `[[`, "type"))
  expect_true("codeminer_col_filters_unmatched" %in% warning_types)
})

test_that("the legacy flat col_filters form returns a structured 422", {
  req <- mock_req(
    '{
      "codes": "all",
      "type": "api_multi",
      "col_filters": {"code": ["disorder"]}
    }'
  )
  res <- new.env()

  response <- col_filters_test_handler(req, res)

  expect_equal(res$status, 422)
  expect_true("codeminer_col_filters_invalid" %in% response$error$error_type)
})
