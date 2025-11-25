test_that("Client functions outputs match equivalent codeminer functions", {
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

  # ---- DESCRIPTION() -----------

  # Compare client and codeminer results
  pattern <- "asthma"
  code_type <- "icd10"

  client_result <- DESCRIPTION(pattern = pattern, code_type = code_type) |>
    suppressMessages()
  direct_result <- codeminer::DESCRIPTION(
    pattern = pattern,
    code_type = code_type
  ) |>
    suppressMessages()

  # Results should be tibbles with data
  expect_equal(
    purrr::map_df(client_result, as.character),
    purrr::map_df(direct_result, as.character)
  )

  # ---- CODES() -----------

  # Test single code
  codes <- "J45"
  code_type <- "icd10"

  client_result <- CODES(codes = codes, code_type = code_type) |>
    suppressMessages()
  direct_result <- codeminer::CODES(codes = codes, code_type = code_type) |>
    suppressMessages()

  expect_equal(
    purrr::map_df(client_result, as.character),
    purrr::map_df(direct_result, as.character)
  )

  # Test vector of codes
  codes <- c("J45", "E11")
  code_type <- "icd10"

  client_result <- CODES(codes = codes, code_type = code_type) |>
    suppressMessages()
  direct_result <- codeminer::CODES(codes = codes, code_type = code_type) |>
    suppressMessages()

  expect_equal(
    purrr::map_df(client_result, as.character),
    purrr::map_df(direct_result, as.character)
  )

  bg$kill()
})

test_that("messages propagate identically", {
  skip_on_cran()

  # --- Build API router manually, including condition test endpoint ---
  pr <- create_codeminer_api()
  pr <- add_condition_test_endpoint(pr)

  # --- Start API in background ---
  bg <- callr::r_bg(
    func = function(pr = pr) {
      plumber::pr_run(
        pr = pr,
        host = "127.0.0.1",
        port = 8893,
        quiet = TRUE
      )
    },
    args = list(
      pr = pr
    ),
    env = c(
      callr::rcmd_safe_env(),
      CODEMINER_DB_PATH = Sys.getenv("CODEMINER_DB_PATH")
    ),
    supervise = TRUE
  )

  # Ensure cleanup
  withr::defer({
    if (bg$is_alive()) bg$kill()
  })

  # --- Wait for server ---
  api_up <- FALSE
  for (i in 1:20) {
    # up to ~4 seconds
    Sys.sleep(0.2)
    try(
      {
        httr2::request("http://127.0.0.1:8893/health") |>
          httr2::req_timeout(0.5) |>
          httr2::req_perform()
        api_up <- TRUE
        break
      },
      silent = TRUE
    )
  }
  if (!api_up) {
    fail("Failed to start API on port 8893")
  }

  withr::local_options(codeminer.api.url = "http://127.0.0.1:8893")

  # --- Capture backend output directly from codeminer ---
  backend_msgs <- capture_messages({
    backend_res <- test_conditions(
      error = FALSE,
      message_class = "codeminer_message",
      warning_class = "codeminer_warning"
    )
  }) |>
    suppressWarnings()

  # --- Capture output through the API ---
  api_msgs <- capture_messages({
    client_res <- test_conditions(
      error = FALSE,
      message_class = "codeminer_message",
      warning_class = "codeminer_warning"
    )
  }) |>
    suppressWarnings()

  # ----- Error raised ----------

  # Check error

  errors_server <- expect_error(
    conditions_test(
      error = TRUE,
      message_class = "codeminer_message",
      warning_class = "codeminer_warning",
      error_class = "codeminer_error"
    ) |>
      suppressWarnings() |>
      suppressMessages()
  )

  errors_client <- expect_error(
    test_conditions(
      error = TRUE,
      message_class = "codeminer_message",
      warning_class = "codeminer_warning",
      error_class = "codeminer_error"
    ) |>
      suppressWarnings() |>
      suppressMessages()
  )

  expect_identical(
    conditionMessage(errors_server),
    conditionMessage(errors_client)
  )

  # Check warning

  warnings_server <- conditions_test(
    error = TRUE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressMessages() |>
    expect_warning() |>
    expect_warning()

  warnings_client <- test_conditions(
    error = TRUE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressMessages() |>
    expect_warning() |>
    expect_warning()

  expect_identical(
    conditionMessage(warnings_server),
    conditionMessage(warnings_client)
  )

  # Check message

  messagess_server <- conditions_test(
    error = TRUE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressWarnings() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message()

  messagess_client <- test_conditions(
    error = TRUE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressWarnings() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message()

  expect_identical(
    conditionMessage(messagess_server),
    conditionMessage(messagess_client)
  )

  # ----- No error raised ----------

  # Check warning

  warnings_server <- conditions_test(
    error = FALSE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressMessages() |>
    expect_warning() |>
    expect_warning()

  warnings_client <- test_conditions(
    error = FALSE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressMessages() |>
    expect_warning() |>
    expect_warning()

  expect_identical(
    conditionMessage(warnings_server),
    conditionMessage(warnings_client)
  )

  # Check message

  messagess_server <- conditions_test(
    error = FALSE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressWarnings() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message()

  messagess_client <- test_conditions(
    error = FALSE,
    message_class = "codeminer_message",
    warning_class = "codeminer_warning",
    error_class = "codeminer_error"
  ) |>
    try(silent = TRUE) |>
    suppressWarnings() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message() |>
    expect_message()

  expect_identical(
    conditionMessage(messagess_server),
    conditionMessage(messagess_client)
  )
})

test_that("Large integer values are read as type character", {
  skip(
    message = paste0(
      "To do - test that large integer values are read as",
      "type character e.g. `100000000000000000000001`"
    )
  )
})
