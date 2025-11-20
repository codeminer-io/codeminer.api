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
    handler = function(req, res, pattern, code_type) {
      tryCatch(
        codeminer::DESCRIPTION(pattern = pattern, code_type = code_type),
        error = function(e) format_backend_error(e, res)
      )
    }
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
    handler = function(req, res, codes, code_type) {
      tryCatch(
        codeminer::CODES(codes = codes, code_type = code_type),
        error = function(e) format_backend_error(e, res)
      )
    }
  )
  pr
}
