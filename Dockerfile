# hadolint ignore=DL3029
FROM --platform=linux/amd64 rocker/tidyverse:4.3.3

# install system dependencies
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    cmake=3.22.* \
  && rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/2024-04-01'))" \
      >> "${R_HOME}/etc/Rprofile.site" \
  && Rscript -e '\
    install.packages("pak", repos = sprintf( \
      "https://r-lib.github.io/p/pak/stable/%s/%s/%s", \
      .Platform["pkgType"], \
      R.Version()["os"], \
      R.Version()["arch"] \
    )) \
    '

WORKDIR /workflow.data.preparation
COPY DESCRIPTION ./

RUN Rscript -e 'pak::local_install_deps()'

COPY .git/ ./.git/
COPY config.yml run_pacta_data_preparation.R ./

CMD ["Rscript", "run_pacta_data_preparation.R"]
