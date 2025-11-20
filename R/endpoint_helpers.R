#' Format backend error as API response
#'
#' Captures error messages raised by server functions (e.g.
#' [codeminer::CODES()]) and also updates http status code.
#'
#' @param e Error object from tryCatch
#' @param res Plumber response object
#' @return List to be serialized as JSON error response
#' @keywords internal
format_backend_error <- function(e, res) {
  warning(
    "Backend error: ",
    conditionMessage(e),
    immediate. = TRUE,
    call. = FALSE
  )
  res$status <- 500
  list(
    error = "Backend Error",
    # use `list()` to propagate cli error message (TODO - custom codeminer error classes)
    message = list(x = conditionMessage(e))
  )
}
