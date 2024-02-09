FROM --platform=linux/amd64 rocker/tidyverse

# install system dependencies for R packages
RUN apt-get update \
    && [ $(which google-chrome) ] || apt-get install -y gnupg curl \
    && [ $(which google-chrome) ] || curl -fsSL -o /tmp/google-chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && [ $(which google-chrome) ] || DEBIAN_FRONTEND='noninteractive' apt-get install -y /tmp/google-chrome.deb \
    && apt-get install -y libcurl4-openssl-dev libssl-dev make libicu-dev libxml2-dev \
    zlib1g-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libjpeg-dev \
    libpng-dev libtiff-dev pandoc git libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages("pak")'

COPY . /workflow.data.preparation

WORKDIR /workflow.data.preparation

RUN Rscript -e '\
  readRenviron(".env"); \
  pak::local_install_deps(); \
  '

CMD Rscript run_pacta_data_preparation.R
