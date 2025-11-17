
<!-- README.md is generated from README.Rmd. Please edit that file -->

# codeminer.api

<!-- badges: start -->

<!-- badges: end -->

`codeminer.api` provides a REST API wrapper for the
[`codeminer`](https://github.com/codeminer-io/codeminer) package,
enabling access to clinical code mappings through HTTP endpoints.

## Installation

Install from GitHub:

``` r
# install.packages("pak")
pak::pak("codeminer-io/codeminer.api")
```

## Quick Start

``` r
library(codeminer.api)

# Set up database path
Sys.setenv(CODEMINER_DB_PATH = "path/to/database.duckdb")

# Start API server
run_codeminer_api()
```

Visit `http://127.0.0.1:8000/__docs__/` for interactive API
documentation.

## Example with Dummy Data

``` r
library(codeminer.api)

# Create temporary database
db_path <- tempfile(fileext = ".duckdb")
codeminer::create_dummy_database(db_path)
Sys.setenv(CODEMINER_DB_PATH = db_path)

# Start API in background
bg <- run_codeminer_api(background = TRUE, port = 8888, quiet = TRUE)

# Query the API
library(httr2)
response <- request("http://127.0.0.1:8888/DESCRIPTION") |>
  req_url_query(pattern = "asthma", code_type = "icd10") |>
  req_perform()

resp_body_json(response)

# Stop server
bg$kill()
```

## Learn More

See `vignette("codeminer.api")` for detailed documentation including:

- Background vs foreground modes
- API endpoint reference
- Process management
- Troubleshooting guide
- Production deployment tips

## API Endpoints

- **`GET /DESCRIPTION`**: Search codes by description pattern
- **`GET /CODES`**: Look up specific codes by code value
- **`GET /__docs__/`**: Interactive API documentation (Swagger UI)
