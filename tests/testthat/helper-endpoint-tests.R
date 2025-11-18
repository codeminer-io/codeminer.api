# Helper function to test endpoint addition
test_endpoint_addition <- function(
  endpoint_function,
  endpoint_name,
  expected_path
) {
  test_that(
    cli::format_inline(
      "{endpoint_name} adds endpoint to router and returns modified router"
    ),
    {
      # Create a mock plumber router
      pr <- plumber::pr()

      # Add endpoint
      pr_modified <- endpoint_function(pr)

      # Check that we got a router back
      expect_s3_class(pr_modified, "Plumber")

      # Check that the endpoint was added
      routes <- pr_modified$routes
      expect_true(length(routes) > 0)

      # Check that the specific endpoint path exists
      paths <- vapply(routes, function(route) route$path, character(1))
      expect_true(
        expected_path %in% paths
      )

      # Should be the same object (modified in place and returned)
      expect_identical(pr, pr_modified)
    }
  )
}
