#' Create CodeMiner API router
#'
#' Constructs a plumber API router with all CodeMiner endpoints attached.
#' This function composes the API by calling individual endpoint helper functions.
#'
#' @return A plumber router object with all endpoints configured
#' @keywords internal
create_codeminer_api <- function() {
  pr <- plumber::pr()

  # Add endpoints
  pr <- add_description_endpoint(pr)
  pr <- add_codes_endpoint(pr)
  # pr <- add_children_endpoint(pr)

  pr
}
