FROM --platform=linux/amd64 rocker/tidyverse

RUN Rscript -e 'install.packages(c("pak", "renv"))'

COPY . /workflow.data.preparation

WORKDIR /workflow.data.preparation

RUN Rscript -e '\
  readRenviron(".env"); \
  non_cran_pkg_deps <- c("RMI-PACTA/pacta.scenario.preparation", "RMI-PACTA/pacta.data.preparation", "RMI-PACTA/pacta.data.scraping"); \
  cran_pkg_deps <- setdiff(renv::dependencies()$Package, basename(non_cran_pkg_deps)); \
  pak::pkg_install(pkg = c(non_cran_pkg_deps, cran_pkg_deps)); \
  '

CMD Rscript run_pacta_data_preparation.R
