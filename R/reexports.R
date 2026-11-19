#' Functions re-exported from codeminer
#'
#' These work the same way whether queries run locally through `codeminer`
#' or over the API through this package, because they read and write the
#' shared `codeminer.*` options that the client functions default from.
#'
#' - [codeminer::with_type()] sets `codeminer.code_type` and
#'   `codeminer.map_to` for the duration of an expression.
#' - [codeminer::with_col_filters()] sets `codeminer.col_filters` for the
#'   duration of an expression, merging its entries into the current value by
#'   table key.
#'
#' @name reexports
#' @keywords internal
#' @importFrom codeminer with_type with_col_filters
#' @export with_type with_col_filters
NULL
