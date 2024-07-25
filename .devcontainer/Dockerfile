FROM rocker/tidyverse:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    librsvg2-dev \
    libglpk-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("languageserver"), repos = "https://cran.rstudio.com/")'