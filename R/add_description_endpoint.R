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
