# Auth strategy abstraction for client-side requests.
#
# A "strategy" is a function `req -> req` (same shape as `httr2::req_auth_*`).
# The `auth_*()` constructors below are factories that return such functions,
# wrapped with metadata for printing and introspection.
#
# Wrapping with metadata (rather than returning a bare function) is what lets
# `auth_use()` store a strategy as a session-wide option and `auth_describe()`
# explain what's currently configured without revealing captured secrets.

new_auth_strategy <- function(fn, kind, description) {
  stopifnot(is.function(fn))
  structure(
    fn,
    class = c("codeminer_auth", "function"),
    auth_kind = kind,
    auth_description = description
  )
}

#' @export
print.codeminer_auth <- function(x, ...) {
  cat("<codeminer_auth>\n")
  cat("  kind: ", attr(x, "auth_kind"), "\n", sep = "")
  cat("  ", attr(x, "auth_description"), "\n", sep = "")
  invisible(x)
}

#' Authentication strategies
#'
#' A strategy is a function that receives an `httr2::request` and returns a
#' (possibly modified) request. It is applied immediately before the request
#' is performed, with URL and body already set, so strategies may modify any
#' aspect of the request.
#'
#' `auth_none()` returns a strategy that leaves the request unchanged.
#'
#' `auth_dev_user()` attaches an `X-Dev-User` header. Only useful when the
#' server is running with `CODEMINER_DEV_AUTH=1` (development setups).
#'
#' `auth_custom()` wraps an arbitrary user-supplied request-modifier function
#' as a strategy with metadata. Use this to plug in auth schemes (bearer
#' tokens fetched from a separate auth service, signed requests, etc.) that
#' the package does not ship built-in. See
#' `vignette("authenticated-requests")` for worked examples.
#'
#' @param user_id Character. User identifier sent in the `X-Dev-User` header.
#'   Defaults to the value of the `CODEMINER_DEV_USER` environment variable.
#' @param fn Function. Takes an `httr2::request` and returns a (possibly
#'   modified) `httr2::request`.
#' @param description Character. Human-readable description of the strategy,
#'   shown by `print()` and `auth_describe()`.
#'
#' @return A function with class `codeminer_auth` that takes an `httr2::request`
#'   and returns a (possibly modified) `httr2::request`.
#'
#' @seealso [auth_use()] to set a strategy session-wide; [auth_current()] and
#'   [auth_describe()] to introspect.
#'
#' @examples
#' \dontrun{
#' # Attach X-Dev-User: alice to outgoing requests for this session
#' auth_use(auth_dev_user("alice"))
#' DESCRIPTION(pattern = "asthma")
#'
#' # Custom strategy: bearer token from an env var
#' auth_use(auth_custom(
#'   function(req) httr2::req_auth_bearer_token(req, Sys.getenv("MY_TOKEN")),
#'   description = "Bearer token from MY_TOKEN env var"
#' ))
#' }
#'
#' @name auth_strategies
NULL

#' @rdname auth_strategies
#' @export
auth_none <- function() {
  new_auth_strategy(
    fn = function(req) req,
    kind = "none",
    description = "No authentication"
  )
}

#' @rdname auth_strategies
#' @export
auth_dev_user <- function(user_id = Sys.getenv("CODEMINER_DEV_USER", "")) {
  force(user_id)
  new_auth_strategy(
    fn = function(req) {
      if (!nzchar(user_id)) {
        return(req)
      }
      httr2::req_headers(req, `X-Dev-User` = user_id)
    },
    kind = "dev_user",
    description = if (nzchar(user_id)) {
      paste0("Dev user '", user_id, "' (X-Dev-User header)")
    } else {
      "Dev user (inactive \u2014 CODEMINER_DEV_USER not set)"
    }
  )
}

#' @rdname auth_strategies
#' @export
auth_custom <- function(fn, description = "Custom auth strategy") {
  stopifnot(is.function(fn))
  new_auth_strategy(fn = fn, kind = "custom", description = description)
}

#' Configure session-wide authentication
#'
#' `auth_use()` sets the auth strategy used by all `codeminer.api` helpers
#' for the rest of the R session. `auth_clear()` removes any session-wide
#' setting, falling back to `default_auth()`.
#'
#' Per-call overrides via the `auth` parameter on each helper still take
#' precedence over the session-wide setting.
#'
#' @param strategy A strategy created by [auth_none()], [auth_dev_user()],
#'   [auth_custom()], or any function with the same shape.
#'
#' @return `auth_use()` returns the strategy invisibly. `auth_clear()` returns
#'   `NULL` invisibly.
#'
#' @examples
#' \dontrun{
#' auth_use(auth_dev_user("alice"))
#' DESCRIPTION(pattern = "asthma")  # uses alice for the rest of the session
#'
#' auth_clear()  # back to default
#' }
#'
#' @export
auth_use <- function(strategy) {
  stopifnot(is.function(strategy))
  options(codeminer.auth = strategy)
  invisible(strategy)
}

#' @rdname auth_use
#' @export
auth_clear <- function() {
  options(codeminer.auth = NULL)
  invisible()
}

#' Inspect the active authentication strategy
#'
#' `auth_current()` returns the strategy that the next API call would use:
#' the session-wide setting from [auth_use()] if set, otherwise the result
#' of [default_auth()].
#'
#' `auth_describe()` returns the human-readable description string for a
#' strategy. Useful for debugging "why is my call returning 401?".
#'
#' `default_auth()` is the fallback resolver. It returns [auth_dev_user()]
#' if `CODEMINER_DEV_USER` is set, otherwise [auth_none()].
#'
#' @param strategy A strategy. Defaults to `auth_current()`.
#'
#' @return `auth_current()` and `default_auth()` return a strategy.
#'   `auth_describe()` returns a character string.
#'
#' @examples
#' \dontrun{
#' auth_current()
#' auth_describe()
#' }
#'
#' @export
auth_current <- function() {
  getOption("codeminer.auth") %||% default_auth()
}

#' @rdname auth_current
#' @export
auth_describe <- function(strategy = auth_current()) {
  desc <- attr(strategy, "auth_description")
  if (is.null(desc)) {
    "Custom auth strategy (no description)"
  } else {
    desc
  }
}

#' @rdname auth_current
#' @export
default_auth <- function() {
  if (nzchar(Sys.getenv("CODEMINER_DEV_USER"))) {
    return(auth_dev_user())
  }
  auth_none()
}
