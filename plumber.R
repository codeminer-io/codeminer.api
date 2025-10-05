# Plumber API for codeminer
# Main API router and configuration

library(plumber)
library(codeminer)
library(jsonlite)
library(logger)

# Configure logging
log_threshold(INFO)

#* @apiTitle CodeMiner API
#* @apiDescription REST API for clinical code mapping using the codeminer R package
#* @apiVersion 0.1.0

# CORS filter
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type,Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

# Authentication filter for customer endpoints
#* @filter auth
auth <- function(req, res) {
  # Skip auth for public endpoints
  if (!grepl("/api/v1/customer/", req$PATH_INFO)) {
    plumber::forward()
    return()
  }
  
  # Check for Authorization header
  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header)) {
    res$status <- 401
    return(list(error = "Authorization header required"))
  }
  
  # TODO: Implement proper PAT validation
  # For now, just check that Bearer token exists
  if (!grepl("^Bearer ", auth_header)) {
    res$status <- 401
    return(list(error = "Invalid authorization format"))
  }
  
  plumber::forward()
}

# Health check endpoint
#* @get /api/v1/health
function() {
  list(
    status = "healthy",
    timestamp = Sys.time(),
    version = packageVersion("codeminer")
  )
}

# Public API endpoints will be implemented here
# Customer API endpoints will be implemented here

# Error handler
#* @errorHandler
function(req, res, err) {
  log_error("API Error: {err$message}")
  res$status <- 500
  list(
    error = "Internal server error",
    message = err$message,
    timestamp = Sys.time()
  )
}
