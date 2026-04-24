FROM rocker/r-ver:4.5.3

# Install pak (binary distribution — no compile dependencies needed)
RUN Rscript -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))' \
 && Rscript -e 'stopifnot(requireNamespace("pak", quietly = TRUE))'

# Copy DESCRIPTION first so the sysreqs + dep-install layers are cached
# independently of changes to R source files
COPY DESCRIPTION /tmp/codeminer.api/DESCRIPTION

# Resolve system dependencies for the full dep tree (including GitHub Remotes)
# and install them via apt. pak queries Posit Package Manager and walks
# Remotes recursively, so transitive sysreqs (e.g. zlib for haven, which
# reaches us via codeminer -> ukbwranglr) are captured.
RUN Rscript -e 'cat(pak::pkg_sysreqs("deps::/tmp/codeminer.api")$install_scripts, sep = "\n", file = "/tmp/sysreqs.sh")' \
 && cat /tmp/sysreqs.sh \
 && apt-get update \
 && bash /tmp/sysreqs.sh \
 && rm -rf /var/lib/apt/lists/* /tmp/sysreqs.sh

# Install all R dependencies (pak handles Remotes automatically)
RUN Rscript -e 'pak::local_install_deps("/tmp/codeminer.api", ask = FALSE)' \
 && Rscript -e 'stopifnot(requireNamespace("codeminer", quietly = TRUE))'

# Copy the rest of the source and install the package itself
COPY . /tmp/codeminer.api
RUN Rscript -e 'pak::local_install("/tmp/codeminer.api", ask = FALSE, upgrade = FALSE)' \
 && Rscript -e 'stopifnot(requireNamespace("codeminer.api", quietly = TRUE))' \
 && rm -rf /tmp/codeminer.api

RUN mkdir -p /data

ENV CODEMINER_DB_PATH=/data/codeminer.duckdb

EXPOSE 8000

CMD ["Rscript", "-e", "codeminer.api::run_codeminer_api(host = '0.0.0.0', port = 8000)"]
