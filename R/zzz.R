.onLoad <- function(libname, pkgname) {
  all_pkg_opts <- list(
    codeminer.api.url = "http://127.0.0.1:8000"
  )

  current_options <- options()

  toset <- !(names(all_pkg_opts) %in% names(current_options))

  if (any(toset)) {
    options(all_pkg_opts[toset])
  }

  invisible()
}
