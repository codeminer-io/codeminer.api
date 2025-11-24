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
    error_message = as.list(e$cli_message)
  )
}

#' Capture a condition's text for warnings/messages
#'
#' Internal helper that extracts a codeminer CLI-formatted message
#' (`cli_message`) condition message. Used by the API
#' handler to collect codeminer warnings and messages uniformly.
#'
#' @keywords internal
#' @noRd
capture_cm_condition <- function(target, warn_env) {
  function(cnd) {
    msg <- if (!is.null(cnd$cli_message)) {
      cnd$cli_message |>
        set_missing_names() |>
        as.list()
    } else {
      conditionMessage(cnd) |>
        set_missing_names()
    }

    warn_env[[target]] <- if (rlang::is_empty(warn_env[[target]])) {
      list(msg)
    } else {
      c(warn_env[[target]], list(msg))
    }

    # muffle if restart exists
    restart <- if (target == "warnings") "muffleWarning" else "muffleMessage"
    # Only muffle when restart is available (e.g., inside withCallingHandlers)
    if (!is.null(findRestart(restart))) {
      invokeRestart(restart)
    }
  }
}

codeminer_handle <- function(expr, res) {
  # setup
  warn_env <- new.env(parent = emptyenv())
  warn_env$warnings <- list()
  warn_env$messages <- list()

  error_response_class <- "error_response"

  # capture warnings and messages (codeminer_* class only)
  response_body <- tryCatch(
    withCallingHandlers(
      expr,
      codeminer_warning = capture_cm_condition("warnings", warn_env),
      codeminer_message = capture_cm_condition("messages", warn_env)
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
      warnings = c(
        list(paste0("Warnings: ", length(warn_env$warnings))),
        warn_env$warnings
      ),
      messages = c(
        list(paste0("Messages: ", length(warn_env$messages))),
        warn_env$messages
      )
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

#' Ensure all elements of a character vector have names
#'
#' Used to guarantee that codeminer messages/warnings/errors, which may be named
#' or unnamed character vectors, are always named for consistent JSON
#' serialisation and round-trip to client-side CLI functions. Unnamed elements
#' are given the name specified by `missing` (default: "-").
#'
#' @param x Character vector (may be named or unnamed)
#' @param missing Name to assign to unnamed elements (default: "-")
#' @return Character vector with all elements named
#' @keywords internal
#' @noRd
set_missing_names <- function(x, missing = "-") {
  nm <- names(x)
  if (is.null(nm)) {
    return(purrr::set_names(x, missing))
  } else {
    nm[nm == "" | is.na(nm)] <- missing
    names(x) <- nm
  }
  x
}
