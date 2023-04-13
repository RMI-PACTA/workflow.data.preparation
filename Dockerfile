FROM --platform=linux/amd64 rocker/tidyverse

RUN Rscript -e 'install.packages(c("pak", "renv"))'

COPY . /workflow.data.preparation

WORKDIR /workflow.data.preparation

RUN Rscript -e '\
  readRenviron(".env"); \
  non_cran_pkgs <- c("RMI-PACTA/pacta.scenario.preparation", "RMI-PACTA/pacta.data.preparation", "RMI-PACTA/pacta.data.scraping"); \
  script_dep_pkgs <- setdiff(renv::dependencies()$Package, basename(non_cran_pkgs)); \
  pak::pkg_install("tidyverse/dbplyr"); \
  pak::pkg_install(pkg = c(non_cran_pkgs, script_dep_pkgs)); \
  '

CMD Rscript run_pacta_data_preparation.R
