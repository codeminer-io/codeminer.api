#' Format backend error as API response
#'
#' Captures error messages raised by server functions (e.g.
#' [codeminer::CODES()]) and also updates http status code.
#'
#' @param e Error object from tryCatch
#' @param res Plumber response object
#' @return List to be serialized as JSON error response
#' @keywords internal
#' @noRd
format_backend_error <- function(e, res) {
  UseMethod("format_backend_error")
}


#' @keywords internal
format_backend_error.default <- function(e, res) {
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

#' @keywords internal
format_backend_error.codeminer_arg_validation_error <- function(e, res) {
  warning(
    "Backend error: ",
    conditionMessage(e),
    immediate. = TRUE,
    call. = FALSE
  )
  # Extract original cli message specification vector
  msg <- e$cli_error_message

  warning(
    "CodeMiner backend error: ",
    conditionMessage(e),
    immediate. = TRUE,
    call. = FALSE
  )

  res$status <- 422

  list(
    error = "CodeMiner Backend Error",
    message = as.list(msg) # preserves "x", "i", "" names
  )
}

codeminer_handle <- function(expr, res) {
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- character()
  warn_env$messages <- character()

  result <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warn_env$warnings <- c(warn_env$warnings, conditionMessage(w))
      },
      message = function(m) {
        warn_env$messages <- c(warn_env$messages, conditionMessage(m))
      }
    ),
    error = function(e) {
      e$warnings <- warn_env$warnings
      e$messages <- warn_env$messages
      return(format_backend_error(e, res))
    }
  )

  list(
    result = result,
    warnings = if (length(warn_env$warnings)) warn_env$warnings else NULL,
    messages = if (length(warn_env$messages)) warn_env$messages else NULL
  )
}

codeminer_handler_factory <- function(f) {
  force(f) # ensures function is captured correctly

  function(req, res, ...) {
    codeminer_handle(f(...), res)
  }
}
