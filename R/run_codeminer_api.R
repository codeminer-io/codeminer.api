#' Run the CodeMiner API
#'
#' @param host Host address (default "127.0.0.1")
#' @param port Port number (default 8000)
#' @param background Logical. Run API in the background? (default FALSE)
#' @param docs Logical. Should swagger docs be enabled? (default TRUE)
#' @param quiet Logical. Suppress startup messages? (default FALSE)
#' @param ... Additional arguments passed to plumber::pr_run()
#'
#' @export
run_codeminer_api <- function(
  host = "127.0.0.1",
  port = 8000,
  background = FALSE,
  docs = TRUE,
  quiet = FALSE,
  ...
) {
  # Check codeminer dependency
  if (!requireNamespace("codeminer", quietly = TRUE)) {
    if (interactive()) {
      message("The 'codeminer' package is required to run the API server.")
      ans <- utils::menu(
        c("Install codeminer now", "Cancel"),
        title = "Missing dependency"
      )
      if (ans == 1) {
        install.packages("codeminer")
      } else {
        stop("Cannot start the API server without the 'codeminer' package.")
      }
    } else {
      stop("The 'codeminer' package is required to run the API server.")
    }
  }

  # Build the API router
  pr <- create_codeminer_api()

  # Docs toggle
  if (!docs) {
    pr <- plumber::pr_set_docs(pr, FALSE)
  }

  # Background mode
  if (background) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop("Background mode requires the 'callr' package.")
    }

    bg <- callr::r_bg(
      func = function(host, port, docs, quiet, ...) {
        library(plumber)
        pr <- codeminer.api::create_codeminer_api()
        if (!docs) pr <- pr_set_docs(pr, FALSE)
        pr_run(pr, host = host, port = port, quiet = quiet, ...)
      },
      args = list(
        host = host,
        port = port,
        docs = docs,
        quiet = quiet
      )
    )

    if (!quiet) {
      message(sprintf(
        "CodeMiner API running in background at http://%s:%s",
        host,
        port
      ))
    }

    return(invisible(bg))
  }

  # Foreground mode
  if (!quiet) {
    message(sprintf("Starting CodeMiner API at http://%s:%s", host, port))
  }

  plumber::pr_run(pr, host = host, port = port, quiet = quiet, ...)
}
