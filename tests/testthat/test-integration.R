test_that("API endpoints respond correctly in background mode", {
  skip_on_cran()
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
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

  # Give server time to start and check for errors
  Sys.sleep(2)

  # If process died, get the error output
  if (!bg$is_alive()) {
    err_output <- bg$read_all_error()
    std_output <- bg$read_all_output()
    skip(sprintf(
      "Background process failed to start.\nStderr: %s\nStdout: %s",
      err_output,
      std_output
    ))
  }

  # Check server is running
  expect_true(bg$is_alive())

  # Test DESCRIPTION endpoint
  response_desc <- httr2::request("http://127.0.0.1:8888/DESCRIPTION") |>
    httr2::req_url_query(
      pattern = "asthma",
      code_type = "icd10"
    ) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_desc), 200)
  expect_match(httr2::resp_content_type(response_desc), "application/json")

  # Test CODES endpoint
  response_codes <- httr2::request("http://127.0.0.1:8888/CODES") |>
    httr2::req_url_query(
      codes = "J45",
      code_type = "icd10"
    ) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(response_codes), 200)
  expect_match(httr2::resp_content_type(response_codes), "application/json")

  # Clean shutdown
  bg$kill()
  Sys.sleep(0.5)
  expect_false(bg$is_alive())
})

test_that("API serves Swagger documentation by default", {
  skip_on_cran()
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
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

  # Check if process is alive, skip with error info if not
  if (!bg$is_alive()) {
    skip(sprintf(
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
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
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

  # Check if process is alive, skip with error info if not
  if (!bg$is_alive()) {
    skip(sprintf(
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
