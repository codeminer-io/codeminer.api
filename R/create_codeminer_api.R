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

  # Add endpoints
  pr <- add_description_endpoint(pr)
  pr <- add_codes_endpoint(pr)
  # pr <- add_children_endpoint(pr)

  pr
}
