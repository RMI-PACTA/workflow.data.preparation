FROM rocker/r-ver

ARG CRAN_REPO="https://packagemanager.posit.co/cran/__linux__/jammy/2024-04-01"
RUN echo "options(repos = c(CRAN = '$CRAN_REPO'))" >> "${R_HOME}/etc/Rprofile.site"

RUN Rscript -e '\
  install.packages("pak", repos = sprintf( \
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s", \
    .Platform$pkgType, \
    R.Version()$os, \
    R.Version()$arch \
  )) \
  '

WORKDIR /workflow.data.preparation
COPY .env DESCRIPTION ./

RUN Rscript -e '\
  readRenviron(".env"); \
  pak::local_install_deps(); \
  '

COPY .git/ ./.git/
COPY config.yml run_pacta_data_preparation.R ./

CMD Rscript run_pacta_data_preparation.R
