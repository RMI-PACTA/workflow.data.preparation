FROM --platform=linux/amd64 rocker/tidyverse

RUN Rscript -e 'install.packages("pak")'

COPY .env /.env
COPY DESCRIPTION /DESCRIPTION

RUN Rscript -e '\
  readRenviron(".env"); \
  pak::local_install_deps(); \
  '

COPY . /

CMD Rscript run_pacta_data_preparation.R
