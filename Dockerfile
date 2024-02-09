FROM --platform=linux/amd64 rocker/tidyverse

RUN Rscript -e '\
  install.packages("pak", repos = sprintf( \
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s", \
    .Platform$pkgType, \
    R.Version()$os, \
    R.Version()$arch \
  )) \
  '

COPY . /workflow.data.preparation

WORKDIR /workflow.data.preparation

RUN Rscript -e '\
  readRenviron(".env"); \
  pak::local_install_deps(); \
  '

CMD Rscript run_pacta_data_preparation.R
