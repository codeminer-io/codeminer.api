#' @inherit codeminer::DESCRIPTION
#' @param .return_raw Logical. If `TRUE`, return raw httr2 response object.
#'   If `FALSE` (default), parse JSON and return as tibble.
#'
#' @examples
#' \dontrun{
#' # Search for ICD-10 codes with descriptions matching "asthma"
#' DESCRIPTION(pattern = "asthma", code_type = "icd10")
#'
#' # Get raw response
#' DESCRIPTION(pattern = "asthma", code_type = "icd10", .return_raw = TRUE)
#' }
#'
#' @seealso [CODES()] for looking up specific codes, [check_api_connection()] for diagnostics
#' @export
DESCRIPTION <- function(pattern, code_type, .return_raw = FALSE) {
  query_params <- list(
    pattern = pattern,
    code_type = code_type
  )

  api_request(
    endpoint = "/DESCRIPTION",
    query_params = query_params,
    .return_raw = .return_raw
  )
}

#' @inherit codeminer::CODES
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' # Look up specific ICD-10 codes
#' CODES(codes = c("J45", "E11"), code_type = "icd10")
#'
#' # Get raw response
#' CODES(codes = "J45", code_type = "icd10", .return_raw = TRUE)
#' }
#'
#' @seealso [DESCRIPTION()] for searching by pattern, [check_api_connection()] for diagnostics
#' @export
CODES <- function(codes, code_type, .return_raw = FALSE) {
  query_params <- list(
    codes = codes,
    code_type = code_type
  )

  api_request(
    endpoint = "/CODES",
    query_params = query_params,
    .return_raw = .return_raw
  )
}
