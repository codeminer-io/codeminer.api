FROM rocker/r-ver:4.4.0

# System dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install remotes for GitHub package installation
RUN R -e 'install.packages("remotes", repos = "https://cloud.r-project.org")'

# Install codeminer + codeminer.api from GitHub
RUN R -e 'remotes::install_github("codeminer-io/codeminer")'
RUN R -e 'remotes::install_github("codeminer-io/codeminer.api")'

# Build dummy database (can be overridden via volume mount)
RUN mkdir -p /data && \
    Rscript -e 'codeminer::create_dummy_database("/data/codeminer.duckdb")'

ENV CODEMINER_DB_PATH=/data/codeminer.duckdb

EXPOSE 8000

CMD ["Rscript", "-e", "codeminer.api::run_codeminer_api(host = '0.0.0.0', port = 8000)"]
