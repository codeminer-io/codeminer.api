test_that("create_codeminer_api creates router with multiple endpoints", {
  pr <- create_codeminer_api()
  
  # Verify it's a valid plumber router
  expect_s3_class(pr, "Plumber")
  
  # Check that routes were added
  routes <- pr$routes
  expect_true(length(routes) > 0)
  
  # Should have at least DESCRIPTION and CODES endpoints
  expect_gte(length(routes), 2)
  
  # Verify specific endpoint paths exist (confirms helpers were invoked)
  paths <- sapply(routes, function(route) route$path)
  expect_true("/DESCRIPTION" %in% paths)
  expect_true("/CODES" %in% paths)
})
