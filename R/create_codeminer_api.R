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
#'
#' @returns error
#' @noRd
#' @export
test <- function() {
  api_request(
    endpoint = "/TEST",
    query_params = NULL,
    .return_raw = FALSE
  )
}

add_test_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/TEST",
    handler = codeminer_handler_factory(
      function() {
        error_message <- c(x = "error", i = "info")
        cli::cli_abort(
          "error",
          class = "codeminer_arg_validation_error",
          cli_error_message = error_message
        )
      }
    ),
  )
  pr
}
