#' Create CodeMiner API router
#'
#' Constructs a plumber API router with all CodeMiner endpoints attached.
#' This function composes the API by calling individual endpoint helper functions.
#'
#' This function is primarily used internally by [run_codeminer_api()], but is
#' exported so it can be accessed by background processes and for advanced users
#' who want to customize the API setup.
#'
#' @return A plumber router object with all endpoints configured
#' @export
create_codeminer_api <- function() {
  pr <- plumber::pr()

  # ensure all columns are returned, even those which are `NA`
  pr$setSerializer(
    plumber::serializer_json(na = "null", dataframe = "rows", keepNA = TRUE)
  )

  # Add endpoints
  pr <- add_health_endpoint(pr)
  pr <- add_description_endpoint(pr)
  pr <- add_codes_endpoint(pr)
  pr <- add_codes_like_endpoint(pr)
  pr <- add_children_endpoint(pr)
  pr <- add_n_children_endpoint(pr)
  pr <- add_parents_endpoint(pr)
  pr <- add_n_parents_endpoint(pr)
  pr <- add_attributes_for_endpoint(pr)
  pr <- add_has_attributes_endpoint(pr)
  pr <- add_relationship_types_from_endpoint(pr)
  pr <- add_relationship_types_to_endpoint(pr)
  pr <- add_map_endpoint(pr)
  pr <- add_metadata_endpoint(pr)

  # Customise OpenAPI spec: POST endpoints use JSON requestBody
  pr$setApiSpec(post_body_api_spec)

  pr
}
