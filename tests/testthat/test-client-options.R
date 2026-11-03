# Record the body each client function sends, without a server
local_capture_requests <- function(.env = parent.frame()) {
  calls <- new.env()
  calls$body <- NULL
  calls$query <- NULL

  local_mocked_bindings(
    api_request = function(query_params = list(), body_params = NULL, ...) {
      calls$body <- body_params
      calls$query <- query_params
      list(result = list())
    },
    .package = "codeminer.api",
    .env = .env
  )

  calls
}

local_unset_codeminer_options <- function(.env = parent.frame()) {
  withr::local_options(
    codeminer.code_type = NULL,
    codeminer.lookup_version = NULL,
    codeminer.relationship_version = NULL,
    codeminer.map_version = NULL,
    codeminer.map_from = NULL,
    codeminer.map_to = NULL,
    codeminer.col_filters = NULL,
    .local_envir = .env
  )
}

# One call per client function taking `type`, each with no `type` argument
type_calls <- list(
  DESCRIPTION = function(...) DESCRIPTION("asthma", ...),
  CODES = function(...) CODES("J45", ...),
  CODES_LIKE = function(...) CODES_LIKE("^J45", ...),
  CHILDREN = function(...) CHILDREN("J45", ...),
  N_CHILDREN = function(...) N_CHILDREN("J45", ...),
  PARENTS = function(...) PARENTS("J45", ...),
  N_PARENTS = function(...) N_PARENTS("J45", ...),
  ATTRIBUTES_FOR = function(...) ATTRIBUTES_FOR("J45", ...),
  HAS_ATTRIBUTES = function(...) HAS_ATTRIBUTES("J45", ...),
  RELATIONSHIP_TYPES = function(...) RELATIONSHIP_TYPES(...),
  RELATIONSHIP_TYPES_FROM = function(...) RELATIONSHIP_TYPES_FROM("J45", ...),
  RELATIONSHIP_TYPES_TO = function(...) RELATIONSHIP_TYPES_TO("J45", ...),
  get_relationship_tree = function(...) get_relationship_tree("J45", ...)
)

test_that("type defaults from codeminer.code_type", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(codeminer.code_type = "ICD-10")

  for (fn in names(type_calls)) {
    calls$body <- NULL
    type_calls[[fn]]()
    expect_identical(calls$body$type, "ICD-10", label = fn)
  }
})

test_that("explicit type overrides codeminer.code_type", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(codeminer.code_type = "ICD-10")

  for (fn in names(type_calls)) {
    calls$body <- NULL
    type_calls[[fn]](type = "Read v3")
    expect_identical(calls$body$type, "Read v3", label = fn)
  }
})

test_that("unset options send a NULL type and latest versions", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()

  for (fn in names(type_calls)) {
    calls$body <- NULL
    type_calls[[fn]]()
    expect_null(calls$body$type, label = fn)
    for (version in c("lookup_version", "relationship_version")) {
      if (version %in% names(calls$body)) {
        expect_identical(calls$body[[version]], "latest", label = fn)
      }
    }
  }
})

test_that("version arguments default from their options", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(
    codeminer.lookup_version = "lkp-1",
    codeminer.relationship_version = "rel-1"
  )

  for (fn in names(type_calls)) {
    calls$body <- NULL
    type_calls[[fn]](type = "ICD-10")
    expect_identical(calls$body$lookup_version, "lkp-1", label = fn)
    if ("relationship_version" %in% names(formals(get(fn)))) {
      expect_identical(calls$body$relationship_version, "rel-1", label = fn)
    }
  }
})

test_that("explicit version arguments override their options", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(
    codeminer.lookup_version = "lkp-1",
    codeminer.relationship_version = "rel-1"
  )

  CHILDREN(
    "J45",
    type = "ICD-10",
    lookup_version = "lkp-2",
    relationship_version = "rel-2"
  )

  expect_identical(calls$body$lookup_version, "lkp-2")
  expect_identical(calls$body$relationship_version, "rel-2")
})

test_that("MAP from, to and versions default from options", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(
    codeminer.map_from = "ICD-10",
    codeminer.map_to = "Read v3",
    codeminer.map_version = "map-1",
    codeminer.lookup_version = "lkp-1"
  )

  MAP("J45")

  expect_identical(calls$body$from, "ICD-10")
  expect_identical(calls$body$to, "Read v3")
  expect_identical(calls$body$map_version, "map-1")
  expect_identical(calls$body$lookup_version, "lkp-1")
})

test_that("explicit MAP arguments override options", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(
    codeminer.map_from = "ICD-10",
    codeminer.map_to = "Read v3",
    codeminer.map_version = "map-1"
  )

  MAP("X40J4", from = "Read v2", to = "BNF", map_version = "map-2")

  expect_identical(calls$body$from, "Read v2")
  expect_identical(calls$body$to, "BNF")
  expect_identical(calls$body$map_version, "map-2")
})

test_that("MAP with unset options sends NULL from/to and latest versions", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()

  MAP("J45")

  expect_null(calls$body$from)
  expect_null(calls$body$to)
  expect_identical(calls$body$map_version, "latest")
  expect_identical(calls$body$lookup_version, "latest")
})

test_that("codeminer.code_type does not supply MAP from or to", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(codeminer.code_type = "ICD-10")

  MAP("J45")

  expect_null(calls$body$from)
  expect_null(calls$body$to)
})

test_that("a codelist supplies MAP from when the option is unset", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  codelist <- data.frame(code = "J45", code_type = "ICD-10")

  MAP(codelist, to = "Read v3")

  expect_identical(calls$body$from, "ICD-10")
})

test_that("a codelist of another type conflicts with codeminer.code_type", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  codelist <- data.frame(code = "J45", code_type = "ICD-10")

  CODES(codelist)
  expect_identical(calls$body$type, "ICD-10")

  withr::local_options(codeminer.code_type = "Read v3")
  expect_error(CODES(codelist), class = "codeminer_error")
})

test_that("codeminer::with_type() reaches the client functions", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()

  codeminer::with_type("ICD-10", DESCRIPTION("asthma"))
  expect_identical(calls$body$type, "ICD-10")

  codeminer::with_type("Read v3", MAP("J45", from = "ICD-10"))
  expect_identical(calls$body$to, "Read v3")
})

test_that("get_codeminer_metadata ignores codeminer.code_type", {
  withr::local_options(
    codeminer.api.url = "http://localhost:8000",
    codeminer.code_type = "ICD-10"
  )
  query <- NULL

  local_mocked_bindings(
    api_request = function(endpoint, query_params = list(), ...) {
      query <<- query_params
      httr2::response(
        status_code = 200,
        headers = "Content-Type: application/json",
        body = charToRaw('{"result": {"code_type": ["ICD-10"]}}')
      )
    },
    .package = "codeminer.api"
  )

  get_codeminer_metadata("lookup")

  expect_identical(query$type, "lookup")
})

# One call per client function taking `col_filters`, each with no `col_filters`
col_filters_calls <- c(
  type_calls,
  # MAP() takes no `type`; swallow it so one loop covers every function
  list(MAP = function(type = NULL, ...) {
    MAP("J45", from = "ICD-10", to = "Read v3", ...)
  })
)

all_edges <- list(relationship = list("SNOMED CT" = NA))

test_that("unset options send today's request bodies", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()

  bodies <- list()
  for (fn in names(col_filters_calls)) {
    calls$body <- NULL
    col_filters_calls[[fn]](type = "ICD-10")
    expect_identical(calls$body$col_filters, "default", label = fn)
    bodies[[fn]] <- calls$body
  }

  # Recorded before col_filters read the option, so this proves an unset
  # option sends exactly the bodies it did then.
  expect_snapshot_value(bodies, style = "deparse")
})

test_that("col_filters defaults from codeminer.col_filters", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(codeminer.col_filters = all_edges)

  for (fn in names(col_filters_calls)) {
    calls$body <- NULL
    col_filters_calls[[fn]](type = "ICD-10")
    expect_identical(calls$body$col_filters, all_edges, label = fn)
  }
})

test_that("explicit col_filters overrides codeminer.col_filters", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(codeminer.col_filters = all_edges)
  other <- list(lookup = list("ICD-10" = list(status = "1")))

  for (fn in names(col_filters_calls)) {
    calls$body <- NULL
    col_filters_calls[[fn]](type = "ICD-10", col_filters = other)
    expect_identical(calls$body$col_filters, other, label = fn)

    calls$body <- NULL
    col_filters_calls[[fn]](type = "ICD-10", col_filters = "default")
    expect_identical(calls$body$col_filters, "default", label = fn)

    calls$body <- NULL
    col_filters_calls[[fn]](type = "ICD-10", col_filters = NULL)
    expect_true("col_filters" %in% names(calls$body), label = fn)
    expect_null(calls$body$col_filters, label = fn)
  }
})

test_that("codeminer.col_filters = NA sends null, like col_filters = NULL", {
  calls <- local_capture_requests()
  local_unset_codeminer_options()
  withr::local_options(codeminer.col_filters = NA)

  for (fn in names(col_filters_calls)) {
    calls$body <- NULL
    col_filters_calls[[fn]](type = "ICD-10")
    expect_true(is.na(calls$body$col_filters), label = fn)
  }

  # Both reach the server as JSON null
  to_json <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null")
  }
  expect_identical(
    to_json(list(col_filters = NA)),
    to_json(list(col_filters = NULL))
  )
  expect_identical(
    as.character(to_json(all_edges)),
    '{"relationship":{"SNOMED CT":null}}'
  )
})
