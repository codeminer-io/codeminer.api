#!/usr/bin/env Rscript
#
# Build a dummy CodeMiner database for testing.
#
# Usage:
#   Rscript build_test_db.R [output_path]
#
# If output_path is omitted, defaults to /data/codeminer.duckdb

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[1] else "/data/codeminer.duckdb"

if (!requireNamespace("codeminer", quietly = TRUE)) {
  stop("Package 'codeminer' is required but not installed.")
}

cat("Building dummy database at:", output_path, "\n")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
codeminer::create_dummy_database(output_path)
cat("Done.\n")
