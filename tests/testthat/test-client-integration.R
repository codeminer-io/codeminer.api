
test_that("Client DESCRIPTION() matches codeminer::DESCRIPTION() via API", {
  skip_on_cran()
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R

  # Start API in background
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8891,
    background = TRUE,
    quiet = TRUE
  )

  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  Sys.sleep(2)

  if (!bg$is_alive()) {
    fail(sprintf(
      "Background process failed. Stderr: %s",
      bg$read_all_error()
    ))
  }

  # Set client API URL
  withr::local_options(codeminer.api.url = "http://127.0.0.1:8891")

  # Compare client and codeminer results
  pattern <- "asthma"
  code_type <- "icd10"

  client_result <- DESCRIPTION(pattern = pattern, code_type = code_type)
  direct_result <- codeminer::DESCRIPTION(pattern = pattern, code_type = code_type)

  # Results should be tibbles with data
  expect_s3_class(client_result, "tbl_df")
  expect_s3_class(direct_result, "tbl_df")
  expect_gt(nrow(client_result), 0)
  expect_gt(nrow(direct_result), 0)

  bg$kill()
})

test_that("Client CODES() matches codeminer::CODES() via API", {
  skip_on_cran()
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R

  # Start API in background
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8892,
    background = TRUE,
    quiet = TRUE
  )

  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  Sys.sleep(2)

  if (!bg$is_alive()) {
    fail(sprintf(
      "Background process failed. Stderr: %s",
      bg$read_all_error()
    ))
  }

  # Set client API URL
  withr::local_options(codeminer.api.url = "http://127.0.0.1:8892")

  # Test single code
  codes <- "J45"
  code_type <- "icd10"

  client_result <- CODES(codes = codes, code_type = code_type)
  direct_result <- codeminer::CODES(codes = codes, code_type = code_type)

  # Results should be tibbles with data
  expect_s3_class(client_result, "tbl_df")
  expect_s3_class(direct_result, "tbl_df")
  expect_gt(nrow(client_result), 0)
  expect_gt(nrow(direct_result), 0)

  bg$kill()
})

test_that("Client functions handle multiple codes correctly", {
  skip_on_cran()
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R

  # Start API in background
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8893,
    background = TRUE,
    quiet = TRUE
  )

  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  Sys.sleep(2)

  if (!bg$is_alive()) {
    fail(sprintf(
      "Background process failed. Stderr: %s",
      bg$read_all_error()
    ))
  }

  # Set client API URL
  withr::local_options(codeminer.api.url = "http://127.0.0.1:8893")

  # Test vector of codes
  codes <- c("J45", "E11", "I10")
  code_type <- "icd10"

  client_result <- CODES(codes = codes, code_type = code_type)
  direct_result <- codeminer::CODES(codes = codes, code_type = code_type)

  expect_equal(as.data.frame(client_result), as.data.frame(direct_result))

  # Should have multiple rows
  expect_true(nrow(client_result) >= 1)

  bg$kill()
})

test_that("check_api_connection works with real API", {
  skip_on_cran()
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R

  # Start API in background
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8894,
    background = TRUE,
    quiet = TRUE
  )

  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  Sys.sleep(2)

  if (!bg$is_alive()) {
    fail(sprintf(
      "Background process failed. Stderr: %s",
      bg$read_all_error()
    ))
  }

  # Wait for API to be ready (with retry logic)
  api_ready <- FALSE
  for (i in 1:10) {
    tryCatch(
      {
        response <- httr2::request("http://127.0.0.1:8894/health") |>
          httr2::req_timeout(2) |>
          httr2::req_perform()
        api_ready <- TRUE
        break
      },
      error = function(e) {
        Sys.sleep(0.5)
      }
    )
  }

  if (!api_ready) {
    fail("API did not become ready in time")
  }

  # Suppress cli output
  withr::local_options(cli.default_handler = function(...) {})

  # Should return TRUE for running API
  result <- check_api_connection("http://127.0.0.1:8894")
  expect_true(result)

  bg$kill()
  Sys.sleep(0.5)

  # Should return FALSE for stopped API
  result <- check_api_connection("http://127.0.0.1:8894")
  expect_false(result)
})

test_that("Client functions return tibbles", {
  skip_on_cran()
  skip_if_not_installed("codeminer")
  skip_if_not_installed("callr")
  skip_if_not_installed("httr2")
  skip_if_offline()

  # Database is set up by tests/testthat/setup.R

  # Start API in background
  bg <- run_codeminer_api(
    host = "127.0.0.1",
    port = 8895,
    background = TRUE,
    quiet = TRUE
  )

  withr::defer({
    if (!is.null(bg) && bg$is_alive()) {
      bg$kill()
    }
  })

  Sys.sleep(2)

  if (!bg$is_alive()) {
    fail(sprintf(
      "Background process failed. Stderr: %s",
      bg$read_all_error()
    ))
  }

  # Set client API URL
  withr::local_options(codeminer.api.url = "http://127.0.0.1:8895")

  # Test DESCRIPTION returns tibble
  desc_result <- DESCRIPTION(pattern = "asthma", code_type = "icd10")
  expect_s3_class(desc_result, "tbl_df")

  # Test CODES returns tibble
  codes_result <- CODES(codes = "J45", code_type = "icd10")
  expect_s3_class(codes_result, "tbl_df")

  bg$kill()
})
