add_description_endpoint <- function(pr) {
  pr$handle(
    "GET",
    "/DESCRIPTION",
    function(pattern, code_type) {
      codeminer::DESCRIPTION(
        pattern = pattern,
        code_type = code_type
      )
    }
  )
  pr
}
