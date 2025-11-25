#' Add health check endpoint to API router
#'
#' Adds a GET endpoint at `/health` that returns API status information.
#' This is a lightweight endpoint for checking if the API is running.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_health_endpoint <- function(pr) {
  pr$handle(
    "GET",
    "/health",
    function() {
      list(
        status = "ok",
        service = "codeminer-api",
        version = as.character(utils::packageVersion("codeminer.api"))
      )
    }
  )
  pr
}

#' Add DESCRIPTION endpoint to API router
#'
#' Adds a GET endpoint at `/DESCRIPTION` that wraps `codeminer::DESCRIPTION()`.
#' Accepts `pattern` (regex pattern) and `code_type` parameters.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_description_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/DESCRIPTION",
    handler = codeminer_handler_factory(
      function(pattern, code_type) {
        codeminer::DESCRIPTION(pattern = pattern, code_type = code_type)
      }
    )
  )
  pr
}

#' Add CODES endpoint to API router
#'
#' Adds a GET endpoint at `/CODES` that wraps `codeminer::CODES()`.
#' Accepts `codes` (comma-separated string or vector) and `code_type` parameters.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_codes_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/CODES",
    handler = codeminer_handler_factory(
      function(codes, code_type) {
        codeminer::CODES(codes = codes, code_type = code_type)
      }
    )
  )
  pr
}

#' Add test endpoint for validating condition behaviour
#'
#' This endpoint is **not** included in the normal API.
#' It is used exclusively in unit tests and integration tests
#' to validate message / warning / error propagation.
#'
#' @param pr A plumber router
#' @return Modified router
#' @keywords internal
#' @noRd
add_condition_test_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/TEST_CONDITIONS",
    handler = codeminer_handler_factory(conditions_test)
  )
  pr
}

conditions_test <- function(
  message_class = "codeminer_message",
  warning_class = "codeminer_warning",
  error_class = "codeminer_error",
  error = TRUE
) {
  # message 1 - 2 bullet points, both the same bullet type
  message_text_1 <- c(
    "i" = "Test message 1a - info bullet",
    "i" = "Test message 1b - also info bullet"
  )

  # message 2 - only one bullet point
  message_text_2 <- c(
    "v" = "Test message 2a - tick bullet",
    "Test message 2b - no bullet"
  )

  # message 3 - no bullet points
  message_text_3 <- c(
    "Test message 3a - no bullet",
    " " = "Test message 3b - indent"
  )

  # message 4 - no bullet points, single message
  message_text_4 <- "Test message 4 - no bullet"

  # message 5 - no bullet points, multiple messages
  message_text_5 <- c(
    "Test message 5a - no bullet",
    "Test message 5b - no bullet"
  )

  if (identical(message_class, "none")) {
    cli::cli_inform(message_text_1)
    cli::cli_inform(message_text_2)
    cli::cli_inform(message_text_3)
    cli::cli_inform(message_text_4)
    cli::cli_inform(message_text_5)
  } else {
    cli::cli_inform(
      message_text_1,
      class = message_class,
      cli_message = message_text_1
    )

    cli::cli_inform(
      message_text_2,
      class = message_class,
      cli_message = message_text_2
    )

    cli::cli_inform(
      message_text_3,
      class = message_class,
      cli_message = message_text_3
    )

    cli::cli_inform(
      message_text_4,
      class = message_class,
      cli_message = message_text_4
    )

    cli::cli_inform(
      message_text_5,
      class = message_class,
      cli_message = message_text_5
    )
  }

  # warning 1 - 2 bullets, different bullet types
  warning_text_1 <- c(
    "!" = "Test warning 1a - warning bullet",
    "*" = "Test warning 1b - round bullet"
  )

  # warning 2 - 3 bullets including an indent bullet
  warning_text_2 <- c(
    "!" = "Test warning 2a - warning bullet",
    "Test warning 2b - no bullet",
    " " = "Test warning 2c - indent"
  )

  if (identical(warning_class, "none")) {
    cli::cli_warn(warning_text_1)
    cli::cli_warn(warning_text_2)
  } else {
    cli::cli_warn(
      message = warning_text_1,
      class = warning_class,
      cli_message = warning_text_1
    )

    cli::cli_warn(
      message = warning_text_2,
      class = warning_class,
      cli_message = warning_text_2
    )
  }

  # error
  if (error) {
    error_text <- c(
      "x" = "Test error - danger bullet",
      ">" = "Test error 2 - arrow bullet"
    )

    if (identical(error_class, "none")) {
      cli::cli_abort(error_text)
    } else {
      cli::cli_abort(
        message = error_text,
        class = error_class,
        cli_message = error_text
      )
    }
  }

  "Success!"
}
