test_that("API endpoints respond correctly in background mode", {
  skip_on_cran()
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R using codeminer::create_dummy_database()

  # Start API in background
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8888,
    background = TRUE,
    quiet = TRUE
  )

  # Ensure cleanup
  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  # Give server time to start
  Sys.sleep(2)

  # Check server is running - fail if it died
  if (!bg$is_alive()) {
    err_output <- bg$read_all_error()
    std_output <- bg$read_all_output()
    fail(sprintf(
      "Background process failed to start.\nStderr: %s\nStdout: %s",
      err_output,
      std_output
    ))
  }

  expect_true(bg$is_alive())

  base_url <- "http://127.0.0.1:8888"

  # Test DESCRIPTION endpoint (POST)
  response_desc <- httr2::request(paste0(base_url, "/DESCRIPTION")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      pattern = "asthma",
      type = "ICD-10"
    )) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_desc), 200)
  expect_match(httr2::resp_content_type(response_desc), "application/json")

  # Test CODES endpoint (POST)
  response_codes <- httr2::request(paste0(base_url, "/CODES")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      codes = "J45",
      type = "ICD-10"
    )) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_codes), 200)
  expect_match(httr2::resp_content_type(response_codes), "application/json")

  # Test CODES_LIKE endpoint (POST)
  response_cl <- httr2::request(paste0(base_url, "/CODES_LIKE")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      pattern = "^J45",
      type = "ICD-10"
    )) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_cl), 200)

  # Test CHILDREN endpoint (POST)
  response_ch <- httr2::request(paste0(base_url, "/CHILDREN")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      codes = "J45",
      type = "ICD-10"
    )) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_ch), 200)

  # Test MAP endpoint (POST)
  response_map <- httr2::request(paste0(base_url, "/MAP")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      codes = "J45",
      from = "ICD-10",
      to = "ICD-9"
    )) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # MAP may return 200 even with empty results, or 422 if no mapping exists
  expect_true(httr2::resp_status(response_map) %in% c(200, 422))

  # Test metadata endpoint (GET)
  response_meta <- httr2::request(paste0(base_url, "/metadata")) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_meta), 200)
  meta_body <- httr2::resp_body_json(response_meta, simplifyVector = TRUE)
  expect_true("result" %in% names(meta_body))

  # Test metadata with type filter
  response_meta_lookup <- httr2::request(paste0(base_url, "/metadata")) |>
    httr2::req_url_query(type = "lookup") |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_meta_lookup), 200)

  # Clean shutdown
  bg$kill()
  Sys.sleep(0.5)
  expect_false(bg$is_alive())
})

test_that("API serves Swagger documentation by default", {
  skip_on_cran()
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R using codeminer::create_dummy_database()

  # Start API with docs enabled (default)
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8889,
    background = TRUE,
    docs = TRUE,
    quiet = TRUE
  )

  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  Sys.sleep(2)

  # Check if process is alive, fail if not
  if (!bg$is_alive()) {
    fail(sprintf(
      "Background process failed. Stderr: %s",
      bg$read_all_error()
    ))
  }

  # Check that __docs__ endpoint exists
  response <- httr2::request("http://127.0.0.1:8889/__docs__/") |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response), 200)

  bg$kill()
})

test_that("API can disable Swagger documentation", {
  skip_on_cran()
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R using codeminer::create_dummy_database()

  # Start API with docs disabled
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8890,
    background = TRUE,
    docs = FALSE,
    quiet = TRUE
  )

  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  Sys.sleep(2)

  # Check if process is alive, fail if not
  if (!bg$is_alive()) {
    fail(sprintf(
      "Background process failed. Stderr: %s",
      bg$read_all_error()
    ))
  }

  # Check that __docs__ endpoint does not exist
  response <- httr2::request("http://127.0.0.1:8890/__docs__/") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response), 404)

  bg$kill()
})
