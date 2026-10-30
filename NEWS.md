# codeminer.api (development version)

* Client functions now default their arguments from the same R options as
  `codeminer`, so one set of options drives both packages: `codeminer.code_type`,
  `codeminer.lookup_version`, `codeminer.relationship_version`,
  `codeminer.map_version`, `codeminer.map_from` and `codeminer.map_to`. Explicit
  arguments still take priority, and nothing changes when these options are
  unset.
