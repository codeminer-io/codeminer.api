#' @inherit codeminer::DESCRIPTION
#' @param col_filters Column filter specification. Use `"default"` for standard
#'   filters, `NULL` for no filtering, or a named list for custom filters.
#' @param .return_raw Logical. If `TRUE`, return raw httr2 response object.
#'   If `FALSE` (default), parse JSON and return as tibble.
#' @param auth Authentication strategy (see [auth_strategies]). Defaults to
#'   [default_auth()], which resolves to the session-wide setting from
#'   [auth_use()] or to env-var-driven defaults. See
#'   `vignette("authenticated-requests")`.
#'
#' @examples
#' \dontrun{
#' DESCRIPTION(pattern = "asthma", type = "icd10")
#' }
#'
#' @seealso [CODES()] for looking up specific codes, [check_api_connection()] for diagnostics
#' @export
DESCRIPTION <- function(
  pattern,
  type = NULL,
  lookup_version = "latest",
  ignore_case = TRUE,
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  body_params <- list(
    pattern = pattern,
    type = type,
    lookup_version = lookup_version,
    ignore_case = ignore_case,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/DESCRIPTION",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::CODES
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' CODES("J45", "E11", type = "icd10")
#' }
#'
#' @seealso [DESCRIPTION()] for searching by pattern, [check_api_connection()] for diagnostics
#' @export
CODES <- function(
  ...,
  type = NULL,
  lookup_version = "latest",
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  body_params <- list(
    codes = collected$codes,
    type = collected$code_type %||% type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/CODES",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::CODES_LIKE
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' CODES_LIKE(pattern = "^J45", type = "icd10")
#' }
#'
#' @export
CODES_LIKE <- function(
  pattern,
  type = NULL,
  lookup_version = "latest",
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  body_params <- list(
    pattern = pattern,
    type = type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/CODES_LIKE",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::CHILDREN
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' CHILDREN("73211009", type = "sct")
#' }
#'
#' @export
CHILDREN <- function(
  ...,
  type = NULL,
  lookup_version = "latest",
  relationship_version = "latest",
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  body_params <- list(
    codes = collected$codes,
    type = collected$code_type %||% type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/CHILDREN",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::N_CHILDREN
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' N_CHILDREN("73211009", depth = 2, type = "sct")
#' }
#'
#' @export
N_CHILDREN <- function(
  ...,
  depth = 1,
  type = NULL,
  lookup_version = "latest",
  relationship_version = "latest",
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  # JSON has no Inf; send as string "Inf"
  depth_json <- if (is.infinite(depth)) "Inf" else depth

  body_params <- list(
    codes = collected$codes,
    depth = depth_json,
    type = collected$code_type %||% type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/N_CHILDREN",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::PARENTS
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' PARENTS("73211009", type = "sct")
#' }
#'
#' @export
PARENTS <- function(
  ...,
  type = NULL,
  lookup_version = "latest",
  relationship_version = "latest",
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  body_params <- list(
    codes = collected$codes,
    type = collected$code_type %||% type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/PARENTS",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::N_PARENTS
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' N_PARENTS("73211009", depth = 2, type = "sct")
#' }
#'
#' @export
N_PARENTS <- function(
  ...,
  depth = 1,
  type = NULL,
  lookup_version = "latest",
  relationship_version = "latest",
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  depth_json <- if (is.infinite(depth)) "Inf" else depth

  body_params <- list(
    codes = collected$codes,
    depth = depth_json,
    type = collected$code_type %||% type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/N_PARENTS",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::ATTRIBUTES_FOR
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' ATTRIBUTES_FOR("73211009", type = "sct")
#' }
#'
#' @export
ATTRIBUTES_FOR <- function(
  ...,
  type = NULL,
  lookup_version = "latest",
  relationship_version = "latest",
  relationship_types = NULL,
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  # Normalise relationship_types: data frames/codelists -> character vector
  if (is.data.frame(relationship_types)) {
    relationship_types <- relationship_types$code
  }

  body_params <- list(
    codes = collected$codes,
    type = collected$code_type %||% type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    relationship_types = relationship_types,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/ATTRIBUTES_FOR",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::HAS_ATTRIBUTES
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' HAS_ATTRIBUTES("73211009", type = "sct")
#' }
#'
#' @export
HAS_ATTRIBUTES <- function(
  ...,
  type = NULL,
  lookup_version = "latest",
  relationship_version = "latest",
  relationship_types = NULL,
  preferred_description_only = TRUE,
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  if (is.data.frame(relationship_types)) {
    relationship_types <- relationship_types$code
  }

  body_params <- list(
    codes = collected$codes,
    type = collected$code_type %||% type,
    lookup_version = lookup_version,
    relationship_version = relationship_version,
    relationship_types = relationship_types,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/HAS_ATTRIBUTES",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::RELATIONSHIP_TYPES_FROM
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' RELATIONSHIP_TYPES_FROM("73211009", type = "sct")
#' }
#'
#' @export
RELATIONSHIP_TYPES_FROM <- function(
  ...,
  type = NULL,
  relationship_version = "latest",
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  body_params <- list(
    codes = collected$codes,
    type = collected$code_type %||% type,
    relationship_version = relationship_version,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/RELATIONSHIP_TYPES_FROM",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::RELATIONSHIP_TYPES_TO
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' RELATIONSHIP_TYPES_TO("73211009", type = "sct")
#' }
#'
#' @export
RELATIONSHIP_TYPES_TO <- function(
  ...,
  type = NULL,
  relationship_version = "latest",
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = type)

  body_params <- list(
    codes = collected$codes,
    type = collected$code_type %||% type,
    relationship_version = relationship_version,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/RELATIONSHIP_TYPES_TO",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' @inherit codeminer::MAP
#' @inheritParams DESCRIPTION
#'
#' @examples
#' \dontrun{
#' MAP("J45", from = "icd10", to = "sct")
#' }
#'
#' @export
MAP <- function(
  ...,
  from = NULL,
  to = NULL,
  map_version = "latest",
  lookup_version = "latest",
  col_filters = "default",
  .return_raw = FALSE,
  auth = default_auth()
) {
  collected <- codeminer::collect_codes_input(..., type = from)

  body_params <- list(
    codes = collected$codes,
    from = collected$code_type %||% from,
    to = to,
    map_version = map_version,
    lookup_version = lookup_version,
    col_filters = col_filters
  )

  api_request(
    endpoint = "/MAP",
    body_params = body_params,
    .return_raw = .return_raw,
    auth = auth
  )
}

#' Get CodeMiner database metadata
#'
#' Returns metadata about the lookup, mapping, and relationship tables
#' available in the CodeMiner database connected to the API server.
#'
#' @param type Character vector of metadata types to return. Must be one or
#'   more of `"lookup"`, `"mapping"`, `"relationship"`. Defaults to all three.
#' @param .return_raw Logical. If `TRUE`, return raw httr2 response object.
#'   If `FALSE` (default), parse JSON and return as list of data frames.
#' @param auth Authentication strategy (see [auth_strategies]). Defaults to
#'   [default_auth()]; see `vignette("authenticated-requests")`.
#'
#' @return If a single type is requested, a data frame. If multiple types are
#'   requested, a named list of data frames.
#'
#' @examples
#' \dontrun{
#' # All metadata
#' get_codeminer_metadata()
#'
#' # Just lookup tables
#' get_codeminer_metadata("lookup")
#'
#' # Lookup and mapping
#' get_codeminer_metadata(c("lookup", "mapping"))
#' }
#'
#' @seealso [check_api_connection()] for diagnostics
#' @export
get_codeminer_metadata <- function(
  type = c("lookup", "mapping", "relationship"),
  .return_raw = FALSE,
  auth = default_auth()
) {
  type <- match.arg(type, several.ok = TRUE)

  query_params <- list(type = paste(type, collapse = ","))

  # Use .return_raw = TRUE because metadata returns a list of data frames,
  # not a single codelist tibble
  response <- api_request(
    endpoint = "/metadata",
    query_params = query_params,
    .return_raw = TRUE,
    auth = auth
  )

  if (.return_raw) {
    return(response)
  }

  # Use simplifyVector = TRUE but handle carefully
  parsed <- httr2::resp_body_json(response, simplifyVector = TRUE)
  print_captured_warnings_and_messages(parsed)

  result <- parsed$result

  # Single type: result is already a data frame from simplifyVector
  if (length(type) == 1L) {
    if (is.data.frame(result)) {
      return(tibble::as_tibble(result))
    }
    # Column-oriented list -> data frame
    return(tibble::as_tibble(
      purrr::map(result, \(col) purrr::map_chr(col, \(x) x %||% NA_character_))
    ))
  }

  # Multiple types: named list of data frames
  purrr::map(result, \(tbl) {
    if (is.data.frame(tbl)) {
      tibble::as_tibble(tbl)
    } else {
      tibble::as_tibble(
        purrr::map(tbl, \(col) purrr::map_chr(col, \(x) x %||% NA_character_))
      )
    }
  })
}

#' Internal client wrapper for condition testing endpoint
#'
#' Not exported. Used only in unit tests.
#'
#' @keywords internal
#' @noRd
test_conditions <- function(
  message_class = "codeminer_message",
  warning_class = "codeminer_warning",
  error_class = "codeminer_error",
  error = TRUE
) {
  query_params <- list(
    message_class = message_class,
    warning_class = warning_class,
    error_class = error_class,
    error = error
  )

  api_request(
    endpoint = "/TEST_CONDITIONS",
    query_params = query_params,
    .return_raw = FALSE
  )
}
