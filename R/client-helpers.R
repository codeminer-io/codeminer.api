#' Validate API URL format
#'
#' @param url Character. API base URL
#' @return The validated URL with trailing slash removed
#' @keywords internal
#' @noRd
validate_api_url <- function(url, call = rlang::caller_env()) {
  if (!rlang::is_string(url)) {
    cli::cli_abort(
      c("API URL must be a single string", "i" = "You supplied: {.type {url}}"),
      call = call
    )
  }

  if (!grepl("^https?://", url)) {
    cli::cli_abort(
      "API URL must start with http:// or https://, got: {.val {url}}",
      call = call
    )
  }

  # Remove trailing slash for consistent endpoint building
  sub("/$", "", url)
}

#' Make a request to the CodeMiner API
#'
#' @param endpoint Character. API endpoint path (e.g., "/DESCRIPTION")
#' @param query_params List. Named list of query parameters
#' @param .return_raw Logical. If `TRUE`, return raw httr2 response object.
#'   If `FALSE` (default), parse JSON and return as tibble.
#'
#' @return A tibble (if `.return_raw = FALSE`) or httr2 response object
#' @keywords internal
#' @noRd
api_request <- function(
  endpoint,
  query_params = list(),
  .return_raw = FALSE,
  call = rlang::caller_env()
) {
  url <- validate_api_url(getOption("codeminer.api.url"), call)
  full_url <- paste0(url, endpoint)

  response <- tryCatch(
    {
      httr2::request(full_url) |>
        httr2::req_url_query(!!!query_params, .multi = "explode") |>
        httr2::req_perform()
    },

    # ---- HANDLE ALL HTTP ERROR CODES HERE (400–599) ----
    httr2_http = function(e) {
      # Try to read structured JSON body from plumber
      parsed <- tryCatch(
        httr2::resp_body_json(e$resp),
        error = function(e2) list(message = conditionMessage(e))
      )

      # Extract structured fields if present
      error_list <- parsed$error$error_message %||% conditionMessage(e)

      # Replay warnings/messages locally
      print_captured_warnings_and_messages(parsed)

      # Raise a structured CLI error
      cli::cli_abort(
        c(
          "x" = "Backend error from CodeMiner API:",
          convert_captured_message_to_cli_message_vector(error_list),
          "i" = paste0("API URL: ", full_url)
        ),
        class = parsed$error$error_type[[1]],
        call = call
      )
    },

    # ---- NON-HTTP ERRORS (timeouts, DNS failures, etc.) ----
    httr2_error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Failed to connect to CodeMiner API",
          "i" = conditionMessage(e),
          "i" = "Check with check_api_connection()",
          "i" = paste0("API URL: ", full_url)
        ),
        call = call
      )
    }
  )

  if (.return_raw) {
    return(response)
  }

  # ---- SUCCESS ----
  parsed <- httr2::resp_body_json(response, simplifyVector = TRUE)

  # Replay warnings/messages in client
  print_captured_warnings_and_messages(parsed)

  tibble::as_tibble(parsed$result)
}

#' Check if CodeMiner API is available
#'
#' Tests connection to the CodeMiner API by calling the /health endpoint.
#' Provides diagnostic information about the connection status.
#'
#' @param url Character. API base URL. Defaults to the configured URL
#'   from `getOption("codeminer.api.url")`.
#'
#' @return Invisibly returns `TRUE` if API is reachable, `FALSE` otherwise.
#'   Also prints diagnostic messages.
#'
#' @examples
#' \dontrun{
#' # Check default API
#' check_api_connection()
#'
#' # Check custom URL
#' check_api_connection("http://myserver:9000")
#' }
#'
#' @export
check_api_connection <- function(
  url = getOption("codeminer.api.url")
) {
  url <- validate_api_url(url)

  tryCatch(
    {
      response <- httr2::request(paste0(url, "/health")) |>
        httr2::req_timeout(5) |>
        httr2::req_perform()

      body <- httr2::resp_body_json(response)
      cli::cli_alert_success("API is available at {.url {url}}")
      cli::cli_alert_info("Service: {body$service}, Version: {body$version}")
      invisible(TRUE)
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to connect to API at {.url {url}}")
      cli::cli_alert_info("Error: {conditionMessage(e)}")
      cli::cli_alert_info(
        "Start API with: {.code run_codeminer_api(background = TRUE)}"
      )
      invisible(FALSE)
    }
  )
}

print_captured_warnings_and_messages <- function(resp) {
  # discard first item in both cases - this just records N warnings/messages
  if (length(resp$warnings)) {
    resp$warnings <- resp$warning[-1] |>
      purrr::map(convert_captured_message_to_cli_message_vector)

    for (w in resp$warnings) cli::cli_warn(message = w)
  }
  if (length(resp$messages)) {
    resp$messages <-
      resp$messages[-1] |>
      purrr::map(convert_captured_message_to_cli_message_vector)

    for (m in resp$messages) cli::cli_inform(message = m)
  }
}

convert_captured_message_to_cli_message_vector <- function(captured_message) {
  if (is.null(names(captured_message))) {
    return(captured_message)
  } else {
    captured_message |>
      purrr::set_names(\(nm) substr(nm, 1, 1)) |>
      unlist(use.names = TRUE)
  }
}
