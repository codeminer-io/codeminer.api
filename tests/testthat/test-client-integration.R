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

  # Helper to compare client and direct results as character columns
  expect_equal_results <- function(client_result, direct_result) {
    expect_equal(
      purrr::map_df(client_result, as.character),
      purrr::map_df(direct_result, as.character)
    )
  }

  # ---- DESCRIPTION() -----------

  client_result <- DESCRIPTION(pattern = "asthma", type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::DESCRIPTION(pattern = "asthma", type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # ---- CODES() -----------

  # Single code
  client_result <- CODES("J45", type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::CODES("J45", type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # Vector of codes
  client_result <- CODES("J45", "E11", type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::CODES("J45", "E11", type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # ---- CODES_LIKE() -----------

  client_result <- CODES_LIKE(pattern = "^J45", type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::CODES_LIKE(pattern = "^J45", type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # ---- CHILDREN() -----------

  client_result <- CHILDREN("J45", type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::CHILDREN("J45", type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # ---- N_CHILDREN() -----------

  client_result <- N_CHILDREN("J45", depth = 1, type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::N_CHILDREN("J45", depth = 1, type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # ---- PARENTS() -----------

  client_result <- PARENTS("J450", type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::PARENTS("J450", type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # ---- N_PARENTS() -----------

  client_result <- N_PARENTS("J450", depth = 1, type = "ICD-10") |>
    suppressMessages()
  direct_result <- codeminer::N_PARENTS("J450", depth = 1, type = "ICD-10") |>
    suppressMessages()

  expect_equal_results(client_result, direct_result)

  # ---- ATTRIBUTES_FOR() -----------

  client_result <- ATTRIBUTES_FOR("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()
  direct_result <- codeminer::ATTRIBUTES_FOR("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()

  expect_equal_results(client_result, direct_result)

  # ---- HAS_ATTRIBUTES() -----------

  client_result <- HAS_ATTRIBUTES("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()
  direct_result <- codeminer::HAS_ATTRIBUTES("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()

  expect_equal_results(client_result, direct_result)

  # ---- RELATIONSHIP_TYPES_FROM() -----------

  client_result <- RELATIONSHIP_TYPES_FROM("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()
  direct_result <- codeminer::RELATIONSHIP_TYPES_FROM("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()

  expect_equal_results(client_result, direct_result)

  # ---- RELATIONSHIP_TYPES_TO() -----------

  client_result <- RELATIONSHIP_TYPES_TO("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()
  direct_result <- codeminer::RELATIONSHIP_TYPES_TO("J45", type = "ICD-10") |>
    suppressMessages() |>
    suppressWarnings()

  expect_equal_results(client_result, direct_result)

  # ---- MAP() -----------

  client_result <- MAP("J45", from = "ICD-10", to = "ICD-9") |>
    suppressMessages() |>
    suppressWarnings()
  direct_result <- codeminer::MAP("J45", from = "ICD-10", to = "ICD-9") |>
    suppressMessages() |>
    suppressWarnings()

  expect_equal_results(client_result, direct_result)

  # ---- get_codeminer_metadata() -----------

  # All metadata
  client_meta <- get_codeminer_metadata()
  direct_meta <- codeminer::get_codeminer_metadata()

  expect_equal(names(client_meta), names(direct_meta))
  for (nm in names(direct_meta)) {
    expect_equal(
      purrr::map_df(client_meta[[nm]], as.character),
      purrr::map_df(direct_meta[[nm]], as.character)
    )
  }

  # Single type
  client_lookup <- get_codeminer_metadata("lookup")
  direct_lookup <- codeminer::get_codeminer_metadata("lookup")

  expect_equal(
    purrr::map_df(client_lookup, as.character),
    purrr::map_df(direct_lookup, as.character)
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
