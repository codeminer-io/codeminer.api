test_that("post_body_api_spec converts POST query params to requestBody", {
  pr <- create_codeminer_api()
  spec <- pr$getApiSpec()

  # All POST endpoints should have requestBody, not parameters
  post_paths <- c(
    "/DESCRIPTION", "/CODES", "/CODES_LIKE", "/CHILDREN", "/N_CHILDREN",
    "/PARENTS", "/N_PARENTS", "/ATTRIBUTES_FOR", "/HAS_ATTRIBUTES",
    "/RELATIONSHIP_TYPES_FROM", "/RELATIONSHIP_TYPES_TO", "/MAP"
  )

  for (path in post_paths) {
    post <- spec$paths[[path]]$post
    expect_true(
      !is.null(post$requestBody),
      info = paste(path, "should have requestBody")
    )
    expect_null(
      post$parameters,
      info = paste(path, "should not have parameters")
    )
    expect_true(
      !is.null(post$requestBody$content$`application/json`$schema),
      info = paste(path, "should have JSON schema")
    )
  }
})

test_that("GET endpoints are not modified by post_body_api_spec", {
  pr <- create_codeminer_api()
  spec <- pr$getApiSpec()

  # /health GET has no parameters and no requestBody
  health <- spec$paths$`/health`$get
  expect_null(health$requestBody)

  # /metadata GET retains its parameters

  metadata <- spec$paths$`/metadata`$get
  expect_null(metadata$requestBody)
  param_names <- vapply(metadata$parameters, \(p) p$name, character(1))
  expect_true("type" %in% param_names)
})

test_that("Schema types are preserved from R defaults", {
  pr <- create_codeminer_api()
  spec <- pr$getApiSpec()

  # DESCRIPTION: preferred_description_only should be boolean
  desc_props <- spec$paths$`/DESCRIPTION`$post$requestBody$content$`application/json`$schema$properties
  expect_equal(desc_props$preferred_description_only$type, "boolean")
  expect_equal(desc_props$preferred_description_only$default, TRUE)

  # DESCRIPTION: ignore_case should be boolean
  expect_equal(desc_props$ignore_case$type, "boolean")

  # DESCRIPTION: lookup_version should be string with default
  expect_equal(desc_props$lookup_version$type, "string")
  expect_equal(desc_props$lookup_version$default, "latest")

  # N_CHILDREN: depth should be number
  nc_props <- spec$paths$`/N_CHILDREN`$post$requestBody$content$`application/json`$schema$properties
  expect_equal(nc_props$depth$type, "number")
  expect_equal(nc_props$depth$default, 1)
})

test_that("Required params are marked in schema", {
  pr <- create_codeminer_api()
  spec <- pr$getApiSpec()

  # DESCRIPTION: pattern is required (no default)
  desc_schema <- spec$paths$`/DESCRIPTION`$post$requestBody$content$`application/json`$schema
  expect_true("pattern" %in% unlist(desc_schema$required))

  # CODES: codes is required
  codes_schema <- spec$paths$`/CODES`$post$requestBody$content$`application/json`$schema
  expect_true("codes" %in% unlist(codes_schema$required))

  # Optional params should not be in required
  expect_false("lookup_version" %in% unlist(desc_schema$required))
  expect_false("type" %in% unlist(desc_schema$required))
})
