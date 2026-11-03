# codeminer.api (development version)

* Client functions now default their arguments from the same R options as
  `codeminer`, so one set of options drives both packages: `codeminer.code_type`,
  `codeminer.lookup_version`, `codeminer.relationship_version`,
  `codeminer.map_version`, `codeminer.map_from` and `codeminer.map_to`. Explicit
  arguments still take priority, and nothing changes when these options are
  unset.

* Client functions also default `col_filters` from the `codeminer.col_filters`
  option, so a script can set
  `options(codeminer.col_filters = list(relationship = list("SNOMED CT" = NA)))`
  once instead of passing it on every call. `NA` turns filtering off (`NULL`
  unsets the option), explicit arguments still take priority, and requests are
  unchanged when the option is unset. A value left in `.Rprofile` from the
  pre-2026 flat form of this option is now sent to the server and rejected.
