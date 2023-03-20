FROM --platform=linux/amd64 rocker/tidyverse

RUN Rscript -e 'install.packages(c("pak", "renv"))'

COPY . /workflow.data.preparation

WORKDIR /workflow.data.preparation

RUN Rscript -e '\
  readRenviron(".env"); \
  private_pkgs <- c("RMI-PACTA/pacta.scenario.preparation", "RMI-PACTA/pacta.data.preparation"); \
  script_dep_pkgs <- setdiff(renv::dependencies()$Package, basename(private_pkgs)); \
  pak::pkg_install("tidyverse/dbplyr@v2.2.1"); \
  pak::pkg_install(pkg = c(private_pkgs, script_dep_pkgs)); \
  '

CMD Rscript run_pacta_data_preparation.R
