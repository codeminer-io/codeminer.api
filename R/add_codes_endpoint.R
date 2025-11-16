add_codes_endpoint <- function(pr) {
  pr$handle(
    method = "GET",
    path = "/CODES",
    handler = function(codes, code_type) {
      codeminer::CODES(
        codes = codes,
        code_type = code_type
      )
    }
  )
  pr
}
