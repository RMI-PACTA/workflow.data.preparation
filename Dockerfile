FROM --platform=linux/amd64 rocker/tidyverse

RUN Rscript -e '\
  install.packages("pak", repos = sprintf( \
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s", \
    .Platform$pkgType, \
    R.Version()$os, \
    R.Version()$arch \
  )) \
  '

WORKDIR /workflow.data.preparation
COPY DESCRIPTION ./

RUN Rscript -e 'pak::local_install_deps()'

COPY .git/ ./.git/
COPY .env config.yml run_pacta_data_preparation.R ./

CMD Rscript run_pacta_data_preparation.R
