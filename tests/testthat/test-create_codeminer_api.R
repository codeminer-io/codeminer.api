test_that("create_codeminer_api creates router with all endpoints", {
  pr <- create_codeminer_api()

  # Verify it's a valid plumber router
  expect_s3_class(pr, "Plumber")

  # Check that routes were added
  routes <- pr$routes
  expect_true(length(routes) > 0)

  # Should have 14 endpoints: health + 12 codeminer functions + metadata
  expect_equal(length(routes), 14)

  # Verify specific endpoint paths exist
  paths <- vapply(routes, function(route) route$path, character(1))
  expect_true("/health" %in% paths)
  expect_true("/DESCRIPTION" %in% paths)
  expect_true("/CODES" %in% paths)
  expect_true("/CODES_LIKE" %in% paths)
  expect_true("/CHILDREN" %in% paths)
  expect_true("/N_CHILDREN" %in% paths)
  expect_true("/PARENTS" %in% paths)
  expect_true("/N_PARENTS" %in% paths)
  expect_true("/ATTRIBUTES_FOR" %in% paths)
  expect_true("/HAS_ATTRIBUTES" %in% paths)
  expect_true("/RELATIONSHIP_TYPES_FROM" %in% paths)
  expect_true("/RELATIONSHIP_TYPES_TO" %in% paths)
  expect_true("/MAP" %in% paths)
  expect_true("/metadata" %in% paths)
})
