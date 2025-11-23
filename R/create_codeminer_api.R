#' Create CodeMiner API router
#'
#' Constructs a plumber API router with all CodeMiner endpoints attached.
#' This function composes the API by calling individual endpoint helper functions.
#'
#' This function is primarily used internally by [run_codeminer_api()], but is
#' exported so it can be accessed by background processes and for advanced users
#' who want to customize the API setup.
#'
#' @return A plumber router object with all endpoints configured
#' @export
create_codeminer_api <- function() {
  pr <- plumber::pr()

  # ensure all columns are returned, even those which are `NA`
  pr$setSerializer(
    plumber::serializer_json(na = "null", dataframe = "rows", keepNA = TRUE)
  )

  # Add endpoints
  pr <- add_health_endpoint(pr)
  pr <- add_description_endpoint(pr)
  pr <- add_codes_endpoint(pr)
  pr <- add_test_endpoint(pr)
  # pr <- add_children_endpoint(pr)

  pr
}

# TESTS -------------------------------------------------------------------

#' Test
add_test_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/TEST",
    handler = codeminer_handler_factory(
      function(
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

        if (identical(message_class, "none")) {
          cli::cli_inform(message_text_1)
        } else {
          cli::cli_inform(
            message_text_1,
            class = message_class,
            cli_message = message_text_1
          )
        }

        # message 2 - only one bullet point
        message_text_2 <- c(
          "v" = "Test message 2a - tick bullet",
          "Test message 2b - no bullet"
        )

        if (identical(message_class, "none")) {
          cli::cli_inform(message_text_2)
        } else {
          cli::cli_inform(
            message_text_2,
            class = message_class,
            cli_message = message_text_2
          )
        }

        # message 3 - no bullet points
        message_text_3 <- c(
          "Test message 3a - no bullet",
          " " = "Test message 3b - indent"
        )

        if (identical(message_class, "none")) {
          cli::cli_inform(message_text_3)
        } else {
          cli::cli_inform(
            message_text_3,
            class = message_class,
            cli_message = message_text_3
          )
        }

        # message 4 - no bullet points, single message
        message_text_4 <- "Test message 4 - no bullet"

        if (identical(message_class, "none")) {
          cli::cli_inform(message_text_4)
        } else {
          cli::cli_inform(
            message_text_4,
            class = message_class,
            cli_message = message_text_4
          )
        }

        # message 5 - no bullet points, multiple messages
        message_text_5 <- c(
          "Test message 5a - no bullet",
          "Test message 5b - no bullet"
        )

        if (identical(message_class, "none")) {
          cli::cli_inform(message_text_5)
        } else {
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

        if (identical(warning_class, "none")) {
          cli::cli_warn(warning_text_1)
        } else {
          cli::cli_warn(
            message = warning_text_1,
            class = warning_class,
            cli_message = warning_text_1
          )
        }

        # warning 2 - 3 bullets including an indent bullet
        warning_text_2 <- c(
          "!" = "Test warning 2a - warning bullet",
          "Test warning 2b - no bullet",
          " " = "Test warning 2c - indent"
        )

        if (identical(warning_class, "none")) {
          cli::cli_warn(warning_text_2)
        } else {
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
    )
  )
  pr
}
