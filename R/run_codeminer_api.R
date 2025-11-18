#' Run the CodeMiner API
#'
#' Start the CodeMiner API server in either foreground or background mode.
#' The API exposes CodeMiner functionality as REST endpoints.
#'
#' @param host Host address (default "127.0.0.1")
#' @param port Port number (default 8000)
#' @param background Logical. Run API in the background? (default FALSE)
#'   When TRUE, returns a process handle that can be used to manage the server.
#' @param docs Logical. Should Swagger docs be enabled? (default TRUE)
#' @param quiet Logical. Suppress startup messages? (default FALSE)
#' @param ... Additional arguments passed to [plumber::pr_run()]
#'
#' @return
#' - In foreground mode: Blocks until server is stopped (Ctrl+C)
#' - In background mode: Returns invisibly a [callr::r_bg] process object
#'
#' @section Background Mode:
#' When `background = TRUE`, the function returns a process handle with methods:
#'
#' - `$is_alive()` - Check if the server is still running
#' - `$kill()` - Stop the server
#' - `$get_pid()` - Get the process ID
#' - `$read_output()` - Read server output
#' - `$read_error()` - Read server errors
#'
#' @section Environment Variables:
#' The `CODEMINER_DB_PATH` environment variable must be set to the path
#' of your CodeMiner database. This is automatically passed from the parent
#' process to the background process.
#'
#' @examples
#' \dontrun{
#' # Start in foreground (blocks until Ctrl+C)
#' run_codeminer_api()
#'
#' # Start in background
#' bg <- run_codeminer_api(background = TRUE)
#'
#' # Check if it's running
#' bg$is_alive()
#'
#' # Stop the server
#' bg$kill()
#'
#' # Custom host and port
#' run_codeminer_api(host = "0.0.0.0", port = 9000)
#' }
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
  # Check CODEMINER_DB_PATH environment variable
  db_path <- Sys.getenv("CODEMINER_DB_PATH", unset = "")

  if (db_path == "") {
    cli::cli_abort(c(
      "The {.envvar CODEMINER_DB_PATH} environment variable is not set.",
      "i" = "Set it in your {.file .Renviron} file:",
      " " = "{.code usethis::edit_r_environ()}",
      " " = "Then add: {.code CODEMINER_DB_PATH=/path/to/database.db}",
      "i" = "Or set it for this session:",
      " " = "{.code Sys.setenv(CODEMINER_DB_PATH = '/path/to/database.db')}"
    ))
  }

  if (!file.exists(db_path)) {
    cli::cli_abort(c(
      "Database file not found at path specified in {.envvar CODEMINER_DB_PATH}.",
      "x" = "Path: {.path {db_path}}",
      "i" = "Check that the path is correct and the file exists."
    ))
  }

  # Build the API router
  pr <- create_codeminer_api()

  # Docs toggle
  if (!docs) {
    pr <- plumber::pr_set_docs(pr, FALSE)
  }

  # Background mode
  if (background) {
    bg <- callr::r_bg(
      func = function(host, port, docs, quiet, env_vars) {
        # Set environment variables in background process
        if (!is.null(env_vars)) {
          do.call(Sys.setenv, as.list(env_vars))
        }

        # Load required packages in the background process
        if (!requireNamespace("plumber", quietly = TRUE)) {
          stop("plumber package not available in background process")
        }
        if (!requireNamespace("codeminer", quietly = TRUE)) {
          stop("codeminer package not available in background process")
        }
        if (!requireNamespace("codeminer.api", quietly = TRUE)) {
          stop("codeminer.api package not available in background process")
        }

        pr <- codeminer.api::create_codeminer_api()
        if (!docs) pr <- plumber::pr_set_docs(pr, FALSE)
        plumber::pr_run(pr, host = host, port = port, quiet = quiet)
      },
      args = list(
        host = host,
        port = port,
        docs = docs,
        quiet = quiet,
        env_vars = c(
          CODEMINER_DB_PATH = Sys.getenv("CODEMINER_DB_PATH")
        )
      ),
      supervise = TRUE
    )
    
    # Give process a moment to start and check if it's alive
    Sys.sleep(0.5)
    
    if (!bg$is_alive()) {
      # Process died, show error
      err_output <- bg$read_all_error()
      cli::cli_abort(c(
        "Failed to start API in background mode.",
        "x" = "Process died immediately after starting.",
        "i" = "Error output:",
        " " = err_output
      ))
    }

    if (!quiet) {
      cli::cli_alert_success(
        "CodeMiner API running in background at {.url http://{host}:{port}}"
      )
      cli::cli_alert_info("Check status: {.code bg$is_alive()}")
      cli::cli_alert_info("Stop server: {.code bg$kill()}")
    }

    return(invisible(bg))
  }

  # Foreground mode
  if (!quiet) {
    cli::cli_alert_info("Starting CodeMiner API at {.url http://{host}:{port}}")
  }

  plumber::pr_run(pr, host = host, port = port, quiet = quiet, ...)
}
