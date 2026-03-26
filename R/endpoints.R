#' Add health check endpoint to API router
#'
#' Adds a GET endpoint at `/health` that returns API status information.
#' This is a lightweight endpoint for checking if the API is running.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_health_endpoint <- function(pr) {
  pr$handle(
    "GET",
    "/health",
    function() {
      list(
        status = "ok",
        service = "codeminer-api",
        version = as.character(utils::packageVersion("codeminer.api"))
      )
    }
  )
  pr
}

#' Add DESCRIPTION endpoint to API router
#'
#' Adds a POST endpoint at `/DESCRIPTION` that wraps `codeminer::DESCRIPTION()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_description_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/DESCRIPTION",
    handler = codeminer_handler_factory(
      function(
        pattern,
        type = NULL,
        lookup_version = "latest",
        ignore_case = TRUE,
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        codeminer::DESCRIPTION(
          pattern = pattern,
          type = type,
          lookup_version = lookup_version,
          ignore_case = ignore_case,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add CODES endpoint to API router
#'
#' Adds a POST endpoint at `/CODES` that wraps `codeminer::CODES()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_codes_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/CODES",
    handler = codeminer_handler_factory(
      function(
        codes,
        type = NULL,
        lookup_version = "latest",
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        codeminer::CODES(
          codes,
          type = type,
          lookup_version = lookup_version,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add CODES_LIKE endpoint to API router
#'
#' Adds a POST endpoint at `/CODES_LIKE` that wraps `codeminer::CODES_LIKE()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_codes_like_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/CODES_LIKE",
    handler = codeminer_handler_factory(
      function(
        pattern,
        type = NULL,
        lookup_version = "latest",
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        codeminer::CODES_LIKE(
          pattern = pattern,
          type = type,
          lookup_version = lookup_version,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add CHILDREN endpoint to API router
#'
#' Adds a POST endpoint at `/CHILDREN` that wraps `codeminer::CHILDREN()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_children_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/CHILDREN",
    handler = codeminer_handler_factory(
      function(
        codes,
        type = NULL,
        lookup_version = "latest",
        relationship_version = "latest",
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        codeminer::CHILDREN(
          codes,
          type = type,
          lookup_version = lookup_version,
          relationship_version = relationship_version,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add N_CHILDREN endpoint to API router
#'
#' Adds a POST endpoint at `/N_CHILDREN` that wraps `codeminer::N_CHILDREN()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_n_children_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/N_CHILDREN",
    handler = codeminer_handler_factory(
      function(
        codes,
        depth = 1,
        type = NULL,
        lookup_version = "latest",
        relationship_version = "latest",
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        # JSON has no Inf; client sends "Inf" as string
        depth <- as.numeric(depth)
        codeminer::N_CHILDREN(
          codes,
          depth = depth,
          type = type,
          lookup_version = lookup_version,
          relationship_version = relationship_version,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add PARENTS endpoint to API router
#'
#' Adds a POST endpoint at `/PARENTS` that wraps `codeminer::PARENTS()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_parents_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/PARENTS",
    handler = codeminer_handler_factory(
      function(
        codes,
        type = NULL,
        lookup_version = "latest",
        relationship_version = "latest",
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        codeminer::PARENTS(
          codes,
          type = type,
          lookup_version = lookup_version,
          relationship_version = relationship_version,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add N_PARENTS endpoint to API router
#'
#' Adds a POST endpoint at `/N_PARENTS` that wraps `codeminer::N_PARENTS()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_n_parents_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/N_PARENTS",
    handler = codeminer_handler_factory(
      function(
        codes,
        depth = 1,
        type = NULL,
        lookup_version = "latest",
        relationship_version = "latest",
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        depth <- as.numeric(depth)
        codeminer::N_PARENTS(
          codes,
          depth = depth,
          type = type,
          lookup_version = lookup_version,
          relationship_version = relationship_version,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add ATTRIBUTES_FOR endpoint to API router
#'
#' Adds a POST endpoint at `/ATTRIBUTES_FOR` that wraps `codeminer::ATTRIBUTES_FOR()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_attributes_for_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/ATTRIBUTES_FOR",
    handler = codeminer_handler_factory(
      function(
        codes,
        type = NULL,
        lookup_version = "latest",
        relationship_version = "latest",
        relationship_types = NULL,
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        codeminer::ATTRIBUTES_FOR(
          codes,
          type = type,
          lookup_version = lookup_version,
          relationship_version = relationship_version,
          relationship_types = relationship_types,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add HAS_ATTRIBUTES endpoint to API router
#'
#' Adds a POST endpoint at `/HAS_ATTRIBUTES` that wraps `codeminer::HAS_ATTRIBUTES()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_has_attributes_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/HAS_ATTRIBUTES",
    handler = codeminer_handler_factory(
      function(
        codes,
        type = NULL,
        lookup_version = "latest",
        relationship_version = "latest",
        relationship_types = NULL,
        preferred_description_only = TRUE,
        col_filters = "default"
      ) {
        codeminer::HAS_ATTRIBUTES(
          codes,
          type = type,
          lookup_version = lookup_version,
          relationship_version = relationship_version,
          relationship_types = relationship_types,
          preferred_description_only = preferred_description_only,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add RELATIONSHIP_TYPES_FROM endpoint to API router
#'
#' Adds a POST endpoint at `/RELATIONSHIP_TYPES_FROM` that wraps
#' `codeminer::RELATIONSHIP_TYPES_FROM()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_relationship_types_from_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/RELATIONSHIP_TYPES_FROM",
    handler = codeminer_handler_factory(
      function(
        codes,
        type = NULL,
        relationship_version = "latest",
        col_filters = "default"
      ) {
        codeminer::RELATIONSHIP_TYPES_FROM(
          codes,
          type = type,
          relationship_version = relationship_version,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add RELATIONSHIP_TYPES_TO endpoint to API router
#'
#' Adds a POST endpoint at `/RELATIONSHIP_TYPES_TO` that wraps
#' `codeminer::RELATIONSHIP_TYPES_TO()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_relationship_types_to_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/RELATIONSHIP_TYPES_TO",
    handler = codeminer_handler_factory(
      function(
        codes,
        type = NULL,
        relationship_version = "latest",
        col_filters = "default"
      ) {
        codeminer::RELATIONSHIP_TYPES_TO(
          codes,
          type = type,
          relationship_version = relationship_version,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add MAP endpoint to API router
#'
#' Adds a POST endpoint at `/MAP` that wraps `codeminer::MAP()`.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_map_endpoint <- function(pr) {
  pr$handle(
    method = "POST",
    path = "/MAP",
    handler = codeminer_handler_factory(
      function(
        codes,
        from = NULL,
        to = NULL,
        map_version = "latest",
        lookup_version = "latest",
        col_filters = "default"
      ) {
        codeminer::MAP(
          codes,
          from = from,
          to = to,
          map_version = map_version,
          lookup_version = lookup_version,
          col_filters = col_filters
        )
      }
    )
  )
  pr
}

#' Add metadata endpoint to API router
#'
#' Adds a GET endpoint at `/metadata` that wraps
#' `codeminer::get_codeminer_metadata()`. Returns metadata about available
#' lookup, mapping, and relationship tables in the database.
#'
#' @param pr A plumber router object
#' @return The modified plumber router
#' @keywords internal
add_metadata_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/metadata",
    handler = codeminer_handler_factory(
      function(type = "lookup,mapping,relationship") {
        # type comes as comma-separated string from query param
        type <- trimws(strsplit(type, ",")[[1]])
        codeminer::get_codeminer_metadata(type = type)
      }
    )
  )
  pr
}

#' Add test endpoint for validating condition behaviour
#'
#' This endpoint is **not** included in the normal API.
#' It is used exclusively in unit tests and integration tests
#' to validate message / warning / error propagation.
#'
#' @param pr A plumber router
#' @return Modified router
#' @keywords internal
#' @noRd
add_condition_test_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/TEST_CONDITIONS",
    handler = codeminer_handler_factory(conditions_test)
  )
  pr
}

conditions_test <- function(
  message_class = "codeminer_message",
  warning_class = "codeminer_warning",
  error_class = "codeminer_error",
  error = TRUE
) {
  # message 1 - 2 bullet points, both the same bullet type
  message_text_1 <- c(
    "i" = "Test message 1a - info bullet",
    "i" = "Test message 1b - also info bullet"
  )

  # message 2 - only one bullet point
  message_text_2 <- c(
    "v" = "Test message 2a - tick bullet",
    "Test message 2b - no bullet"
  )

  # message 3 - no bullet points
  message_text_3 <- c(
    "Test message 3a - no bullet",
    " " = "Test message 3b - indent"
  )

  # message 4 - no bullet points, single message
  message_text_4 <- "Test message 4 - no bullet"

  # message 5 - no bullet points, multiple messages
  message_text_5 <- c(
    "Test message 5a - no bullet",
    "Test message 5b - no bullet"
  )

  if (identical(message_class, "none")) {
    cli::cli_inform(message_text_1)
    cli::cli_inform(message_text_2)
    cli::cli_inform(message_text_3)
    cli::cli_inform(message_text_4)
    cli::cli_inform(message_text_5)
  } else {
    cli::cli_inform(
      message_text_1,
      class = message_class,
      cli_message = message_text_1
    )

    cli::cli_inform(
      message_text_2,
      class = message_class,
      cli_message = message_text_2
    )

    cli::cli_inform(
      message_text_3,
      class = message_class,
      cli_message = message_text_3
    )

    cli::cli_inform(
      message_text_4,
      class = message_class,
      cli_message = message_text_4
    )

    cli::cli_inform(
      message_text_5,
      class = message_class,
      cli_message = message_text_5
    )
  }

  # warning 1 - 2 bullets, different bullet types
  warning_text_1 <- c(
    "!" = "Test warning 1a - warning bullet",
    "*" = "Test warning 1b - round bullet"
  )

  # warning 2 - 3 bullets including an indent bullet
  warning_text_2 <- c(
    "!" = "Test warning 2a - warning bullet",
    "Test warning 2b - no bullet",
    " " = "Test warning 2c - indent"
  )

  if (identical(warning_class, "none")) {
    cli::cli_warn(warning_text_1)
    cli::cli_warn(warning_text_2)
  } else {
    cli::cli_warn(
      message = warning_text_1,
      class = warning_class,
      cli_message = warning_text_1
    )

    cli::cli_warn(
      message = warning_text_2,
      class = warning_class,
      cli_message = warning_text_2
    )
  }

  # error
  if (error) {
    error_text <- c(
      "x" = "Test error - danger bullet",
      ">" = "Test error 2 - arrow bullet"
    )

    if (identical(error_class, "none")) {
      cli::cli_abort(error_text)
    } else {
      cli::cli_abort(
        message = error_text,
        class = error_class,
        cli_message = error_text
      )
    }
  }

  "Success!"
}
