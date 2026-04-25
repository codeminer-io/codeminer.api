# Tests for the client-side auth strategy abstraction.
# These tests exercise strategies and helpers without firing real HTTP calls;
# we either inspect the modified httr2 request object directly or mock
# `httr2::req_perform()` to capture what api_request() would send.

# ---- Strategy constructors ---------------------------------------------------

test_that("auth_none() leaves the request unchanged", {
  req <- httr2::request("http://example.com")
  out <- auth_none()(req)

  expect_identical(out$headers, req$headers)
})

test_that("auth_dev_user() with explicit user_id sets X-Dev-User header", {
  req <- httr2::request("http://example.com")
  out <- auth_dev_user("alice")(req)

  expect_equal(out$headers[["X-Dev-User"]], "alice")
})

test_that("auth_dev_user() reads CODEMINER_DEV_USER when no arg given", {
  withr::local_envvar(CODEMINER_DEV_USER = "bob")

  req <- httr2::request("http://example.com")
  out <- auth_dev_user()(req)

  expect_equal(out$headers[["X-Dev-User"]], "bob")
})

test_that("auth_dev_user() with no arg and no env var attaches no header", {
  withr::local_envvar(CODEMINER_DEV_USER = "")

  req <- httr2::request("http://example.com")
  out <- auth_dev_user()(req)

  expect_null(out$headers[["X-Dev-User"]])
})

test_that("auth_custom() wraps an arbitrary request modifier", {
  fn <- function(req) httr2::req_headers(req, `X-Test` = "yes")
  strategy <- auth_custom(fn, description = "test strategy")

  req <- httr2::request("http://example.com")
  out <- strategy(req)

  expect_equal(out$headers[["X-Test"]], "yes")
})

# ---- S3 class and printing ---------------------------------------------------

test_that("strategies inherit codeminer_auth and function classes", {
  expect_s3_class(auth_none(), c("codeminer_auth", "function"), exact = TRUE)
  expect_s3_class(
    auth_dev_user("alice"),
    c("codeminer_auth", "function"),
    exact = TRUE
  )
  expect_s3_class(
    auth_custom(function(req) req),
    c("codeminer_auth", "function"),
    exact = TRUE
  )
})

test_that("print method displays kind and description", {
  expect_output(print(auth_none()), "kind: none")
  expect_output(print(auth_none()), "No authentication")

  expect_output(print(auth_dev_user("alice")), "kind: dev_user")
  expect_output(print(auth_dev_user("alice")), "Dev user 'alice'")
})

test_that("auth_describe() returns the description string", {
  expect_equal(auth_describe(auth_none()), "No authentication")
  expect_equal(
    auth_describe(auth_dev_user("alice")),
    "Dev user 'alice' (X-Dev-User header)"
  )
})

test_that("auth_describe() handles custom strategies without description", {
  bare <- structure(
    function(req) req,
    class = c("codeminer_auth", "function")
  )
  expect_equal(
    auth_describe(bare),
    "Custom auth strategy (no description)"
  )
})

# ---- Session-wide configuration ----------------------------------------------

test_that("auth_use() sets and auth_clear() unsets the option", {
  withr::local_options(codeminer.auth = NULL)

  auth_use(auth_dev_user("alice"))
  expect_true(is.function(getOption("codeminer.auth")))

  auth_clear()
  expect_null(getOption("codeminer.auth"))
})

test_that("auth_current() returns session-wide setting when set", {
  withr::local_options(codeminer.auth = NULL)
  withr::local_envvar(CODEMINER_DEV_USER = "")

  auth_use(auth_dev_user("bob"))
  expect_equal(attr(auth_current(), "auth_kind"), "dev_user")
  expect_equal(auth_describe(), "Dev user 'bob' (X-Dev-User header)")
})

test_that("auth_current() falls back to default_auth() when nothing is set", {
  withr::local_options(codeminer.auth = NULL)
  withr::local_envvar(CODEMINER_DEV_USER = "")

  expect_equal(attr(auth_current(), "auth_kind"), "none")
})

# ---- default_auth() resolver -------------------------------------------------

test_that("default_auth() returns auth_none() when CODEMINER_DEV_USER unset", {
  withr::local_envvar(CODEMINER_DEV_USER = "")

  expect_equal(attr(default_auth(), "auth_kind"), "none")
})

test_that("default_auth() picks auth_dev_user() when DEV_USER env set", {
  withr::local_envvar(CODEMINER_DEV_USER = "carol")

  strategy <- default_auth()
  expect_equal(attr(strategy, "auth_kind"), "dev_user")

  req <- httr2::request("http://example.com")
  expect_equal(strategy(req)$headers[["X-Dev-User"]], "carol")
})

# ---- Helper integration ------------------------------------------------------
# These tests intercept httr2::req_perform() and assert on the captured
# request headers. They confirm the full path:
#   helper -> api_request -> strategy applied.

mock_perform_capturing <- function(captured_env) {
  function(req) {
    captured_env$req <- req
    structure(
      list(
        status_code = 200,
        headers = list(`content-type` = "application/json"),
        body = charToRaw('{"result": []}')
      ),
      class = "httr2_response"
    )
  }
}

mock_resp_body <- function(
  resp,
  simplifyVector = FALSE, # nolint: object_name_linter.
  bigint_as_char = FALSE
) {
  list(result = data.frame())
}

test_that("DESCRIPTION() with no auth arg uses default_auth() lazily", {
  withr::local_options(codeminer.api.url = "http://example.com")
  withr::local_options(codeminer.auth = NULL)
  withr::local_envvar(CODEMINER_DEV_USER = "")

  captured <- new.env()
  local_mocked_bindings(
    req_perform = mock_perform_capturing(captured),
    resp_body_json = mock_resp_body,
    .package = "httr2"
  )

  DESCRIPTION(pattern = "asthma")

  expect_null(captured$req$headers[["X-Dev-User"]])
  expect_null(captured$req$headers[["Authorization"]])
})

test_that("DESCRIPTION() picks up auth_use() session setting at call time", {
  withr::local_options(codeminer.api.url = "http://example.com")
  withr::local_options(codeminer.auth = NULL)
  withr::local_envvar(CODEMINER_DEV_USER = "")

  auth_use(auth_dev_user("alice"))
  withr::defer(auth_clear())

  captured <- new.env()
  local_mocked_bindings(
    req_perform = mock_perform_capturing(captured),
    resp_body_json = mock_resp_body,
    .package = "httr2"
  )

  DESCRIPTION(pattern = "asthma")

  expect_equal(captured$req$headers[["X-Dev-User"]], "alice")
})

test_that("DESCRIPTION() picks up DEV_USER env var via default_auth()", {
  withr::local_options(codeminer.api.url = "http://example.com")
  withr::local_options(codeminer.auth = NULL)
  withr::local_envvar(CODEMINER_DEV_USER = "dave")

  captured <- new.env()
  local_mocked_bindings(
    req_perform = mock_perform_capturing(captured),
    resp_body_json = mock_resp_body,
    .package = "httr2"
  )

  DESCRIPTION(pattern = "asthma")

  expect_equal(captured$req$headers[["X-Dev-User"]], "dave")
})

test_that("per-call auth = ... overrides the session setting", {
  withr::local_options(codeminer.api.url = "http://example.com")
  withr::local_options(codeminer.auth = NULL)
  withr::local_envvar(CODEMINER_DEV_USER = "")

  auth_use(auth_dev_user("alice"))
  withr::defer(auth_clear())

  captured <- new.env()
  local_mocked_bindings(
    req_perform = mock_perform_capturing(captured),
    resp_body_json = mock_resp_body,
    .package = "httr2"
  )

  DESCRIPTION(pattern = "asthma", auth = auth_dev_user("admin"))

  expect_equal(captured$req$headers[["X-Dev-User"]], "admin")
})

test_that("custom auth strategy is applied to outgoing requests", {
  withr::local_options(codeminer.api.url = "http://example.com")
  withr::local_options(codeminer.auth = NULL)
  withr::local_envvar(CODEMINER_DEV_USER = "")

  custom <- auth_custom(
    function(req) httr2::req_headers(req, `X-Custom` = "yes"),
    description = "test"
  )

  captured <- new.env()
  local_mocked_bindings(
    req_perform = mock_perform_capturing(captured),
    resp_body_json = mock_resp_body,
    .package = "httr2"
  )

  DESCRIPTION(pattern = "asthma", auth = custom)

  expect_equal(captured$req$headers[["X-Custom"]], "yes")
})
