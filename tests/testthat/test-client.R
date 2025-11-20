test_that("DESCRIPTION calls api_request with correct parameters", {
  # Track calls to api_request
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params, .return_raw) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(
          endpoint = endpoint,
          query_params = query_params,
          return_raw = .return_raw
        ))
      )
      # Return mock tibble
      tibble::tibble(code = "J45", description = "Asthma", code_type = "icd10")
    },
    .package = "codeminer.api"
  )

  result <- DESCRIPTION(pattern = "asthma", code_type = "icd10")

  expect_length(api_request_calls, 1)
  expect_equal(api_request_calls[[1]]$endpoint, "/DESCRIPTION")
  expect_equal(api_request_calls[[1]]$query_params$pattern, "asthma")
  expect_equal(api_request_calls[[1]]$query_params$code_type, "icd10")
  expect_false(api_request_calls[[1]]$return_raw)
})

test_that("DESCRIPTION passes .return_raw to api_request", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params, .return_raw) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(
          return_raw = .return_raw
        ))
      )
      structure(list(status_code = 200), class = "httr2_response")
    },
    .package = "codeminer.api"
  )

  result <- DESCRIPTION(
    pattern = "asthma",
    code_type = "icd10",
    .return_raw = TRUE
  )

  expect_true(api_request_calls[[1]]$return_raw)
})


test_that("CODES accepts single code string", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params, .return_raw) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(
          endpoint = endpoint,
          query_params = query_params
        ))
      )
      tibble::tibble(code = "J45", description = "Asthma", code_type = "icd10")
    },
    .package = "codeminer.api"
  )

  result <- CODES(codes = "J45", code_type = "icd10")

  expect_length(api_request_calls, 1)
  expect_equal(api_request_calls[[1]]$endpoint, "/CODES")
  expect_equal(api_request_calls[[1]]$query_params$codes, "J45")
  expect_equal(api_request_calls[[1]]$query_params$code_type, "icd10")
})

test_that("CODES accepts vector of codes", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params, .return_raw) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(
          query_params = query_params
        ))
      )
      tibble::tibble(
        code = c("J45", "E11"),
        description = c("Asthma", "Type 2 diabetes"),
        code_type = "icd10"
      )
    },
    .package = "codeminer.api"
  )

  result <- CODES(codes = c("J45", "E11"), code_type = "icd10")

  expect_equal(api_request_calls[[1]]$query_params$codes, c("J45", "E11"))
})

test_that("CODES passes .return_raw to api_request", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params, .return_raw) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(
          return_raw = .return_raw
        ))
      )
      structure(list(status_code = 200), class = "httr2_response")
    },
    .package = "codeminer.api"
  )

  result <- CODES(codes = "J45", code_type = "icd10", .return_raw = TRUE)

  expect_true(api_request_calls[[1]]$return_raw)
})
