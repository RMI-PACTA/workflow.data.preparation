dependencies <- c(
  "DBI",
  "RSQLite",
  "config",
  "dplyr",
  "readr",
  "rlang",
  "rlog",
  # "stats", # base package, do not update
  "stringr",
  "tidyr"
)

github_dependencies <- c(
  "RMI-PACTA/pacta.data.preparation",
  "RMI-PACTA/pacta.data.scraping",
  "RMI-PACTA/pacta.scenario.preparation"
)

# get github_pat from docker build secrets
github_pat <- readLines("/run/secrets/github_pat")
if (!nzchar(github_pat)) {
  stop("github_pat secret is empty. Is it being passed in build secrets?")
}

install.packages(
  pkgs = dependencies,
  repos = "https://packagemanager.posit.co/cran/__linux__/jammy/2023-08-31",
  dependencies = c("Depends", "Imports", "LinkingTo")
)

# remotes available as part of rocker/tidyverse
remotes::install_github(
  repo = github_dependencies,
  auth_token = github_pat,
  dependencies = c("Depends", "Imports", "LinkingTo"),
  repos = "https://packagemanager.posit.co/cran/__linux__/jammy/2023-08-31",
  upgrade = "always"
)
