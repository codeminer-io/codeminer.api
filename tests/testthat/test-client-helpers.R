test_that("validate_api_url rejects invalid URLs", {
  # Not a string
  expect_error(
    validate_api_url(123),
    "must be a single string"
  )

  # Multiple strings
  expect_error(
    validate_api_url(c("http://test.com", "http://test2.com")),
    "must be a single string"
  )

  # Missing http/https
  expect_error(
    validate_api_url("test.com"),
    "must start with http:// or https://"
  )
})

test_that("validate_api_url removes trailing slash", {
  expect_equal(
    validate_api_url("http://localhost:8000/"),
    "http://localhost:8000"
  )

  expect_equal(
    validate_api_url("https://api.example.com/"),
    "https://api.example.com"
  )
})

test_that("validate_api_url accepts valid URLs", {
  expect_equal(
    validate_api_url("http://localhost:8000"),
    "http://localhost:8000"
  )

  expect_equal(
    validate_api_url("https://api.example.com"),
    "https://api.example.com"
  )
})

test_that("api_request handles connection errors gracefully", {
  withr::local_options(codeminer.api.url = "http://nonexistent.local:9999")

  # Should wrap httr2 error with helpful message
  expect_error(
    api_request("/DESCRIPTION", list(pattern = "test", code_type = "icd10")),
    "Failed to connect"
  )
})

test_that("api_request parses JSON to tibble by default", {
  skip_if_not_installed("httr2")

  withr::local_options(codeminer.api.url = "http://localhost:8000")

  # Mock httr2::req_perform to return a successful response
  local_mocked_bindings(
    req_perform = function(req) {
      # Create mock response with JSON body
      structure(
        list(),
        class = "httr2_response"
      )
    },
    .package = "httr2"
  )

  # Also mock resp_body_json
  local_mocked_bindings(
    resp_body_json = function(resp, simplifyVector = FALSE) {
      list(
        result = data.frame(
          code = "J45",
          description = "Asthma",
          code_type = "icd10",
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "httr2"
  )

  result <- api_request(
    "/DESCRIPTION",
    list(pattern = "asthma", code_type = "icd10")
  )

  expect_s3_class(result, "tbl_df")
  expect_true("code" %in% names(result))
})

test_that("api_request returns raw response when .return_raw = TRUE", {
  skip_if_not_installed("httr2")

  withr::local_options(codeminer.api.url = "http://localhost:8000")

  # Mock httr2::req_perform
  mock_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json"),
      body = charToRaw('[{"code": "J45"}]')
    ),
    class = "httr2_response"
  )

  local_mocked_bindings(
    req_perform = function(req) mock_response,
    .package = "httr2"
  )

  result <- api_request(
    "/DESCRIPTION",
    list(pattern = "asthma", code_type = "icd10"),
    .return_raw = TRUE
  )

  expect_s3_class(result, "httr2_response")
})

test_that("check_api_connection returns TRUE for successful connection", {
  skip_if_not_installed("httr2")

  # Mock successful health check
  local_mocked_bindings(
    req_perform = function(req) {
      structure(
        list(
          status_code = 200,
          headers = list(`content-type` = "application/json"),
          body = charToRaw(
            '{"status": "ok", "service": "codeminer-api", "version": "0.0.0.9000"}'
          )
        ),
        class = "httr2_response"
      )
    },
    .package = "httr2"
  )

  local_mocked_bindings(
    resp_body_json = function(resp) {
      list(status = "ok", service = "codeminer-api", version = "0.0.0.9000")
    },
    .package = "httr2"
  )

  # Suppress cli output
  withr::local_options(cli.default_handler = function(...) {
  })

  result <- check_api_connection("http://localhost:8000")
  expect_true(result)
})

test_that("check_api_connection returns FALSE for failed connection", {
  skip_if_not_installed("httr2")

  # Mock failed connection
  local_mocked_bindings(
    req_perform = function(req) {
      stop("Connection refused")
    },
    .package = "httr2"
  )

  # Suppress cli output
  withr::local_options(cli.default_handler = function(...) {
  })

  result <- check_api_connection("http://nonexistent.local:9999")
  expect_false(result)
})
