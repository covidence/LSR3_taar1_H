FROM ghcr.io/rocker-org/devcontainer/tidyverse:4

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    librsvg2-dev \
    libglpk-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN cat <<EOF > /etc/rstudio/logging.conf
[*]
log-level=debug
logger-type=file
log-dir=/var/log/rstudio/rstudio-server
log-file-mode=0644
EOF
