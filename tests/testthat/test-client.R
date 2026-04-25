test_that("DESCRIPTION calls api_request with correct POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(
          endpoint = endpoint,
          body_params = body_params,
          return_raw = .return_raw
        ))
      )
      tibble::tibble(code = "J45", description = "Asthma", code_type = "icd10")
    },
    .package = "codeminer.api"
  )

  result <- DESCRIPTION(pattern = "asthma", type = "icd10")

  expect_length(api_request_calls, 1)
  expect_equal(api_request_calls[[1]]$endpoint, "/DESCRIPTION")
  expect_equal(api_request_calls[[1]]$body_params$pattern, "asthma")
  expect_equal(api_request_calls[[1]]$body_params$type, "icd10")
  expect_false(api_request_calls[[1]]$return_raw)
})

test_that("DESCRIPTION passes .return_raw to api_request", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(return_raw = .return_raw))
      )
      structure(list(status_code = 200), class = "httr2_response")
    },
    .package = "codeminer.api"
  )

  result <- DESCRIPTION(pattern = "asthma", type = "icd10", .return_raw = TRUE)
  expect_true(api_request_calls[[1]]$return_raw)
})

test_that("DESCRIPTION passes all parameters", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(body_params = body_params)))
      tibble::tibble(code = "J45", description = "Asthma", code_type = "icd10")
    },
    .package = "codeminer.api"
  )

  DESCRIPTION(
    pattern = "asthma",
    type = "icd10",
    lookup_version = "v2",
    ignore_case = FALSE,
    preferred_description_only = FALSE,
    col_filters = NULL
  )

  bp <- api_request_calls[[1]]$body_params
  expect_equal(bp$lookup_version, "v2")
  expect_false(bp$ignore_case)
  expect_false(bp$preferred_description_only)
  expect_null(bp$col_filters)
})

test_that("CODES accepts codes via ...", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(
        api_request_calls,
        list(list(endpoint = endpoint, body_params = body_params))
      )
      tibble::tibble(code = "J45", description = "Asthma", code_type = "icd10")
    },
    .package = "codeminer.api"
  )

  result <- CODES("J45", type = "icd10")

  expect_length(api_request_calls, 1)
  expect_equal(api_request_calls[[1]]$endpoint, "/CODES")
  expect_equal(api_request_calls[[1]]$body_params$codes, "J45")
  expect_equal(api_request_calls[[1]]$body_params$type, "icd10")
})

test_that("CODES accepts vector of codes", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(body_params = body_params)))
      tibble::tibble(
        code = c("J45", "E11"),
        description = c("Asthma", "Type 2 diabetes"),
        code_type = "icd10"
      )
    },
    .package = "codeminer.api"
  )

  result <- CODES("J45", "E11", type = "icd10")
  expect_equal(api_request_calls[[1]]$body_params$codes, c("J45", "E11"))
})

test_that("CODES passes .return_raw to api_request", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(return_raw = .return_raw)))
      structure(list(status_code = 200), class = "httr2_response")
    },
    .package = "codeminer.api"
  )

  result <- CODES("J45", type = "icd10", .return_raw = TRUE)
  expect_true(api_request_calls[[1]]$return_raw)
})

test_that("CODES_LIKE calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(code = "J450", description = "Asthma", code_type = "icd10")
    },
    .package = "codeminer.api"
  )

  CODES_LIKE(pattern = "^J45", type = "icd10")
  expect_equal(api_request_calls[[1]]$endpoint, "/CODES_LIKE")
  expect_equal(api_request_calls[[1]]$body_params$pattern, "^J45")
  expect_equal(api_request_calls[[1]]$body_params$type, "icd10")
})

test_that("CHILDREN calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(code = "child1", description = "Child", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  CHILDREN("73211009", type = "sct")
  expect_equal(api_request_calls[[1]]$endpoint, "/CHILDREN")
  expect_equal(api_request_calls[[1]]$body_params$codes, "73211009")
  expect_equal(api_request_calls[[1]]$body_params$type, "sct")
})

test_that("N_CHILDREN sends depth as string for Inf", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(body_params = body_params)))
      tibble::tibble(code = "child1", description = "Child", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  N_CHILDREN("73211009", depth = Inf, type = "sct")
  expect_equal(api_request_calls[[1]]$body_params$depth, "Inf")
})

test_that("N_CHILDREN sends numeric depth for finite values", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(body_params = body_params)))
      tibble::tibble(code = "child1", description = "Child", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  N_CHILDREN("73211009", depth = 2, type = "sct")
  expect_equal(api_request_calls[[1]]$body_params$depth, 2)
})

test_that("PARENTS calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(code = "parent1", description = "Parent", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  PARENTS("73211009", type = "sct")
  expect_equal(api_request_calls[[1]]$endpoint, "/PARENTS")
  expect_equal(api_request_calls[[1]]$body_params$codes, "73211009")
})

test_that("N_PARENTS sends depth as string for Inf", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(body_params = body_params)))
      tibble::tibble(code = "parent1", description = "Parent", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  N_PARENTS("73211009", depth = Inf, type = "sct")
  expect_equal(api_request_calls[[1]]$body_params$depth, "Inf")
})

test_that("ATTRIBUTES_FOR calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(code = "attr1", description = "Attribute", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  ATTRIBUTES_FOR("73211009", type = "sct", relationship_types = c("Is a"))
  expect_equal(api_request_calls[[1]]$endpoint, "/ATTRIBUTES_FOR")
  expect_equal(api_request_calls[[1]]$body_params$relationship_types, c("Is a"))
})

test_that("ATTRIBUTES_FOR normalises data frame relationship_types to character", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(body_params = body_params)))
      tibble::tibble(code = "attr1", description = "Attribute", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  rt_df <- data.frame(code = c("Is a", "Has finding site"), code_type = "sct")
  ATTRIBUTES_FOR("73211009", type = "sct", relationship_types = rt_df)
  expect_equal(
    api_request_calls[[1]]$body_params$relationship_types,
    c("Is a", "Has finding site")
  )
})

test_that("HAS_ATTRIBUTES calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(code = "code1", description = "Has attr", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  HAS_ATTRIBUTES("73211009", type = "sct")
  expect_equal(api_request_calls[[1]]$endpoint, "/HAS_ATTRIBUTES")
})

test_that("RELATIONSHIP_TYPES_FROM calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(result = "Is a")
    },
    .package = "codeminer.api"
  )

  RELATIONSHIP_TYPES_FROM("73211009", type = "sct")
  expect_equal(api_request_calls[[1]]$endpoint, "/RELATIONSHIP_TYPES_FROM")
  expect_equal(api_request_calls[[1]]$body_params$codes, "73211009")
})

test_that("RELATIONSHIP_TYPES_TO calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(result = "Is a")
    },
    .package = "codeminer.api"
  )

  RELATIONSHIP_TYPES_TO("73211009", type = "sct")
  expect_equal(api_request_calls[[1]]$endpoint, "/RELATIONSHIP_TYPES_TO")
})

test_that("MAP calls correct endpoint with POST body", {
  api_request_calls <- list()

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(endpoint = endpoint, body_params = body_params)))
      tibble::tibble(code = "mapped", description = "Mapped", code_type = "sct")
    },
    .package = "codeminer.api"
  )

  MAP("J45", from = "icd10", to = "sct")
  expect_equal(api_request_calls[[1]]$endpoint, "/MAP")
  expect_equal(api_request_calls[[1]]$body_params$codes, "J45")
  expect_equal(api_request_calls[[1]]$body_params$from, "icd10")
  expect_equal(api_request_calls[[1]]$body_params$to, "sct")
})

test_that("get_codeminer_metadata calls correct endpoint with query params", {
  api_request_calls <- list()

  # get_codeminer_metadata uses .return_raw = TRUE internally, so mock must return
  # a real httr2 response. We use httr2::response() to build one.
  mock_json <- '{"result":{"code_type":["ICD-10"],"lookup_version":["v0"]},"warnings":[["Warnings: 0"]],"messages":[["Messages: 0"]]}'

  mock_response <- httr2::response(
    status_code = 200L,
    headers = list(`content-type` = "application/json"),
    body = charToRaw(mock_json)
  )

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), body_params = NULL, .return_raw = FALSE, auth = auth_current(), call = rlang::caller_env()) {
      api_request_calls <<- c(api_request_calls, list(list(
        endpoint = endpoint,
        query_params = query_params,
        return_raw = .return_raw
      )))
      mock_response
    },
    .package = "codeminer.api"
  )

  result <- get_codeminer_metadata("lookup")
  expect_length(api_request_calls, 1)
  expect_equal(api_request_calls[[1]]$endpoint, "/metadata")
  expect_equal(api_request_calls[[1]]$query_params$type, "lookup")
  expect_true(api_request_calls[[1]]$return_raw)
})
