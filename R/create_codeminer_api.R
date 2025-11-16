create_codeminer_api <- function() {
  pr <- plumber::pr()

  # Add endpoints
  pr <- add_description_endpoint(pr)
  pr <- add_codes_endpoint(pr)
  # pr <- add_children_endpoint(pr)

  pr
}
