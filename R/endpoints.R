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
    "GET",
    "/DESCRIPTION",
    function(pattern, code_type) {
      codeminer::DESCRIPTION(
        pattern = pattern,
        code_type = code_type
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
    handler = function(codes, code_type) {
      codeminer::CODES(
        codes = codes,
        code_type = code_type
      )
    }
  )
  pr
}
