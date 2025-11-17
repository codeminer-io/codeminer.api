test_that("run_codeminer_api fails when codeminer not available", {
  # Mock file.exists to return TRUE (skip db check)
  local_mocked_bindings(
    file.exists = function(path) TRUE,
    .package = "base"
  )
  
  # Mock check_package_available to return FALSE
  local_mocked_bindings(
    check_package_available = function(pkg) FALSE,
    .package = "codeminer.api"
  )

  expect_error(
    run_codeminer_api(),
    "codeminer.*required"
  )
})

test_that("run_codeminer_api fails when callr not available for background mode", {
  # Mock file.exists to return TRUE (skip db check)
  local_mocked_bindings(
    file.exists = function(path) TRUE,
    .package = "base"
  )
  
  # Mock: codeminer available, callr not available
  local_mocked_bindings(
    check_package_available = function(pkg) {
      if (pkg == "codeminer") return(TRUE)
      if (pkg == "callr") return(FALSE)
      FALSE
    },
    create_codeminer_api = function() plumber::pr(),
    .package = "codeminer.api"
  )

  expect_error(
    run_codeminer_api(background = TRUE),
    "requires the callr package"
  )
})

test_that("run_codeminer_api creates router and calls pr_run in foreground mode", {
  # Track if pr_run was called and with what arguments
  pr_run_called <- FALSE
  pr_run_args <- NULL

  # Mock file.exists to return TRUE (skip db check)
  local_mocked_bindings(
    file.exists = function(path) TRUE,
    .package = "base"
  )

  local_mocked_bindings(
    check_package_available = function(pkg) TRUE,
    create_codeminer_api = function() plumber::pr(),
    .package = "codeminer.api"
  )

  # Mock plumber::pr_run
  withr::local_options(list(
    testthat_mock_plumber_pr_run = TRUE
  ))

  local_mocked_bindings(
    pr_run = function(pr, ...) {
      pr_run_called <<- TRUE
      pr_run_args <<- list(...)
      NULL
    },
    .package = "plumber"
  )

  # Run in foreground with quiet mode
  run_codeminer_api(host = "127.0.0.1", port = 9000, quiet = TRUE)

  # Verify pr_run was called
  expect_true(pr_run_called)
  expect_equal(pr_run_args$host, "127.0.0.1")
  expect_equal(pr_run_args$port, 9000)
  expect_equal(pr_run_args$quiet, TRUE)
})

test_that("run_codeminer_api disables docs when docs = FALSE", {
  # Track if pr_set_docs was called
  pr_set_docs_called <- FALSE
  pr_set_docs_args <- NULL

  # Mock file.exists to return TRUE (skip db check)
  local_mocked_bindings(
    file.exists = function(path) TRUE,
    .package = "base"
  )

  local_mocked_bindings(
    check_package_available = function(pkg) TRUE,
    create_codeminer_api = function() plumber::pr(),
    .package = "codeminer.api"
  )

  local_mocked_bindings(
    pr_set_docs = function(pr, value) {
      pr_set_docs_called <<- TRUE
      pr_set_docs_args <<- value
      pr
    },
    pr_run = function(...) NULL,
    .package = "plumber"
  )

  # Run with docs disabled
  run_codeminer_api(docs = FALSE, quiet = TRUE)

  # Verify pr_set_docs was called with FALSE
  expect_true(pr_set_docs_called)
  expect_equal(pr_set_docs_args, FALSE)
})

test_that("run_codeminer_api calls callr::r_bg in background mode", {
  # Track if r_bg was called
  r_bg_called <- FALSE
  mock_bg_process <- list(is_alive = function() TRUE)

  # Mock file.exists to return TRUE (skip db check)
  local_mocked_bindings(
    file.exists = function(path) TRUE,
    .package = "base"
  )

  local_mocked_bindings(
    check_package_available = function(pkg) TRUE,
    create_codeminer_api = function() plumber::pr(),
    .package = "codeminer.api"
  )

  local_mocked_bindings(
    r_bg = function(func, args, ...) {
      r_bg_called <<- TRUE
      mock_bg_process
    },
    .package = "callr"
  )

  # Run in background mode
  result <- run_codeminer_api(background = TRUE, quiet = TRUE)

  # Verify r_bg was called
  expect_true(r_bg_called)

  # Verify result is the background process
  expect_equal(result, mock_bg_process)
})
