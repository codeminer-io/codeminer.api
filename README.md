# codeminer-api

REST API service for clinical code mapping using the codeminer R package.

## Overview

This repository provides a Plumber-based REST API that exposes the functionality of the `codeminer` R package as web endpoints. It enables:

- **Public API endpoints** for general clinical code mapping
- **Customer-specific endpoints** with authentication for premium features
- **Scalable deployment** options for SaaS hosting
- **Direct integration** with the core codeminer package

## Features

- 🔍 **Code Lookup**: Search and validate clinical codes
- 🔄 **Code Mapping**: Transform between coding systems  
- 🌳 **Hierarchical Queries**: Navigate code hierarchies
- 🔐 **Authentication**: PAT-based access control for customers
- 📊 **Customer Data**: Individual DuckDB instances per customer
- 📝 **Documentation**: Interactive API documentation

## API Endpoints

### Public Endpoints
- `GET /api/v1/codes/lookup` - Look up code information
- `GET /api/v1/codes/map` - Map between coding systems
- `GET /api/v1/codes/children` - Get child codes
- `GET /api/v1/health` - Health check endpoint

### Customer Endpoints (PAT required)
- `POST /api/v1/customer/codelists` - Manage custom codelists
- `GET /api/v1/customer/data` - Access customer-specific data
- `POST /api/v1/customer/bulk` - Bulk processing operations

## Development

### Prerequisites
- R (>= 4.1.0)
- codeminer package
- Plumber
- Docker (for containerized deployment)

### Local Development

```r
# Install dependencies
devtools::install_deps()

# Start the API server
source("plumber.R")
```

The API will be available at `http://localhost:8000`

### Docker Deployment

```bash
docker build -t codeminer-api .
docker run -p 8000:8000 codeminer-api
```

## Authentication

Customer endpoints require a Personal Access Token (PAT) in the `Authorization` header:

```
Authorization: Bearer <your-pat-token>
```

## Configuration

Environment variables:
- `CODEMINER_DB_PATH`: Path to the main database
- `CUSTOMER_DB_DIR`: Directory for customer databases  
- `JWT_SECRET`: Secret for token validation
- `LOG_LEVEL`: Logging level (DEBUG, INFO, WARN, ERROR)

## Related Projects

- **[codeminer](https://github.com/your-org/codeminer)**: Core R package
- **[codeminer-py](https://github.com/your-org/codeminer-py)**: Python client library
- **[codeminer-app](https://github.com/your-org/codeminer-app)**: Web application

## License

AGPL-3 © Alasdair Warwick
