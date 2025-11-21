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
    error_type = "Backend Error",
    error_message = list(x = conditionMessage(e))
  )
}

#' @keywords internal
format_backend_error.codeminer_error <- function(e, res) {
  warning(
    "Backend error: ",
    conditionMessage(e),
    immediate. = TRUE,
    call. = FALSE
  )

  warning(
    "CodeMiner backend error: ",
    conditionMessage(e),
    immediate. = TRUE,
    call. = FALSE
  )

  res$status <- 422

  list(
    error_type = class(e)[1],
    # Extract original cli message specification vector - preserves "x", "i", "" names
    error_message = as.list(e$cli_error_message)
  )
}

codeminer_handle <- function(expr, res) {
  # setup
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- character()
  warn_env$messages <- character()

  error_response_class <- "error_response"

  # capture warnings and messages
  response_body <- tryCatch(
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
      # format errors and set `error_response_class`
      error_response <- format_backend_error(e, res)
      class(error_response) <- error_response_class
      return(error_response)
    }
  )

  # determine whether request was successful or raised an error
  result_or_error_name <- ifelse(
    inherits(response_body, error_response_class),
    "error",
    "result"
  )

  # response body will be a list with items result/error, warnings, messages
  c(
    stats::setNames(list(unclass(response_body)), result_or_error_name),
    list(
      warnings = if (length(warn_env$warnings)) warn_env$warnings else NULL,
      messages = if (length(warn_env$messages)) warn_env$messages else NULL
    )
  )
}

codeminer_handler_factory <- function(f) {
  force(f) # ensures function is captured correctly

  # req is unused; required by plumber
  function(req, res, ...) {
    codeminer_handle(f(...), res)
  }
}
