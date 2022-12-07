FROM rocker/tidyverse

RUN Rscript -e 'install.packages("devtools")'

RUN --mount=type=secret,id=GITHUB_PAT \
  Rscript -e 'devtools::install_github("RMI-PACTA/pacta.scenario.preparation", auth_token = readLines("/run/secrets/GITHUB_PAT"))'

RUN --mount=type=secret,id=GITHUB_PAT \
  Rscript -e 'devtools::install_github("RMI-PACTA/pacta.data.preparation", auth_token = readLines("/run/secrets/GITHUB_PAT"))'

COPY . /workflow.data.preparation
WORKDIR /workflow.data.preparation

CMD Rscript --vanilla /workflow.data.preparation/run_pacta_data_preparation.R | tee /home/shiny/outputs/file.log
