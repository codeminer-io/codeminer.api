#' Format backend errors for API responses
#'
#' Internal generic used by `codeminer_handle()` to convert backend errors
#' raised inside plumber endpoints into structured JSON responses. This
#' mechanism determines the HTTP status code, the JSON structure returned to
#' clients, and whether anything is logged to the server console.
#'
#' The API distinguishes between:
#'
#' - **Unexpected backend errors** (e.g., internal bugs, database issues)
#'   -> handled by `format_backend_error.default()`
#'
#' - **Expected user-facing CodeMiner errors** raised via [codeminer::codeminer_abort()]
#'   -> handled by `format_backend_error.codeminer_error()`
#'
#' Warnings and messages are handled separately by `codeminer_handle()`, but
#' only if they have class `codeminer_warning` or `codeminer_message`. Base R
#' warnings/messages are not captured and never exposed to the client.
#'
#' @param e A condition object passed from the API handler.
#' @param res A plumber response object whose `status` field will be updated.
#'
#' @return A named list with items `error_type` and `error_message`, that
#'   plumber will serialise to JSON. The latter should be a named list of one or
#'   more error messages where names can be used as bullet points with
#'   [cli::cli_abort()] (see [cli::cli_bullets()] for valid bullet types). If no
#'   bullet point is needed, then use '-' (ignored by [cli::cli_abort()]).
#'
#' @keywords internal
#' @noRd
format_backend_error <- function(e, res) {
  UseMethod("format_backend_error")
}

#' Default formatter for unexpected backend errors
#'
#' Handles non-CodeMiner errors that escape from endpoint execution. These
#' represent internal/unexpected failures (e.g. coding errors, missing fields,
#' database corruption) and are *not* intended for end-users.
#'
#' Behaviour:
#' - Writes a warning to server stderr using [warning()], ensuring the issue
#' appears in the API logs.
#' - Sets HTTP status 500.
#' - Returns a generic error payload to the client. Only the high-level
#' condition message is exposed.
#' - Base R warnings/messages are not captured by the API handler and will not
#' be replayed to the client.
#'
#' @inheritParams format_backend_error
#'
#' @return A list suitable for JSON serialisation, containing:
#'   - `error_type` (always "Backend Error")
#'   - `error_message` (a named list containing the plain error message with name "x")
#'
#' @keywords internal
#' @noRd
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

#' Formatter for CodeMiner user-facing errors
#'
#' Handles errors raised via `codeminer_abort()`, which represent expected,
#' user-facing validation failures (e.g. invalid arguments, malformed inputs).
#'
#' Behaviour:
#' - **Does not log** a server warning (avoids log noise for anticipated errors).
#' - Sets HTTP status 422.
#' - Extracts the structured CLI message vector stored in the condition's
#'   `cli_message` attribute. This allows the client wrappers to faithfully
#'   recreate the exact bullet list produced by `[cli_abort()]`.
#'
#' Only errors with class `codeminer_error` use this method. All other errors
#' fall back to `format_backend_error.default()`.
#'
#' @inheritParams format_backend_error
#'
#' @return A list suitable for JSON serialisation, containing:
#'   - `error_type` – the most specific CodeMiner error class (e.g.,
#'     `"codeminer_arg_validation_error"`)
#'   - `error_message` – the stored CLI message vector (as a named list)
#'
#' @keywords internal
#' @noRd
format_backend_error.codeminer_error <- function(e, res) {
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

  # Get the original function's formals (parameter list)
  original_formals <- rlang::fn_fmls(f)

  # Create wrapper that preserves signature for Swagger
  wrapper <- function(req, res) {
    # Extract args that match f's parameters from query and/or POST body
    all_args <- c(req$args, req$body)
    param_names <- intersect(rlang::fn_fmls_names(f), names(all_args))
    args <- all_args[param_names]
    codeminer_handle(rlang::exec(f, !!!args), res)
  }

  # Set the wrapper's formals to match the original function
  # This is what Swagger will inspect
  rlang::fn_fmls(wrapper) <- c(
    rlang::pairlist2(req = , res = ),
    original_formals
  )

  wrapper
}

#' Customise OpenAPI spec so POST endpoints use JSON request body
#'
#' Plumber auto-generates OpenAPI specs with all handler parameters as query
#' params, even for POST endpoints. This function post-processes the spec to
#' convert query parameters into a `requestBody` with JSON schema for POST
#' methods, so Swagger UI renders a JSON body editor instead of individual
#' query param inputs.
#'
#' GET endpoints are left unchanged.
#'
#' @param spec The auto-generated OpenAPI spec (a nested list)
#' @return The modified spec
#' @keywords internal
#' @noRd
post_body_api_spec <- function(spec) {
  for (path_name in names(spec$paths)) {
    post <- spec$paths[[path_name]]$post
    if (is.null(post)) next

    params <- post$parameters
    if (is.null(params) || length(params) == 0) next

    properties <- list()
    required <- character()

    for (p in params) {
      if (!identical(p[["in"]], "query")) next
      properties[[p$name]] <- p$schema
      if (isTRUE(p$required)) required <- c(required, p$name)
    }

    schema <- list(type = "object", properties = properties)
    if (length(required) > 0) schema$required <- as.list(required)

    post$requestBody <- list(
      required = TRUE,
      content = list(`application/json` = list(schema = schema))
    )
    post$parameters <- NULL
    spec$paths[[path_name]]$post <- post
  }
  spec
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
