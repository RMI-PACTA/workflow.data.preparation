# workflow.data.preparation

`workflow.data.preparation` orchestrates the PACTA data preparation process, combining production, financial, scenario, and currency data into a format suitable for use in a PACTA for investors analysis. Assuming that the computing resource being used has sufficient memory (which can be >16gb depending on the inputs), storage space, and access to the necessary inputs, this is intended to work on a desktop or laptop using RStudio or run using the included [Dockerfile](https://github.com/RMI-PACTA/workflow.data.preparation/blob/main/Dockerfile) and [docker-compose.yml](https://github.com/RMI-PACTA/workflow.data.preparation/blob/main/docker-compose.yml).

## Running in RStudio

### R package dependencies

Running workflow.data.preparation has a number of R package dependencies that are listed in the DESCRIPTION file. These can be installed manually or by using something like `pak::local_install_deps()`.

### Setting appropriate config options

To make things easier, the recommended way to specify the desired config set when running locally in RStudio is by setting the active config set to `desktop` and modifying/adding only a few of the properties in the `desktop` config set. By doing so, you benefit from inheriting many of the appropriate configuration values without having to explicitly specify each one.

You will need to set the `inherits` parameter, e.g. `inherits: 2022Q4`, to select which of the config sets specified in the config.yml file that is desired. 

You will need to set `data_prep_outputs_path` to an *existing* directory where you want the outputs to be saved, e.g. `data_prep_outputs_path: "./outputs"` to point to an existing directory named `outputs` in the working directory of the R session you will be running data.prep in. This directory must exist before running data.prep (and ideally be empty). The script will throw an error early on if it does not exist.

You will need to set `asset_impact_data_path` to the locally accessible directory where the necessary asset data files are located (absolute, or relative to the working directory of the R session you will be running data.prep in).

You will need to set `factset_data_path` to the locally accessible directory where the necessary financial data files are located (absolute, or relative to the working directory of the R session you will be running data.prep in).

### Setting the active config set

Before you begin, you must set the active config in an open R session with `Sys.setenv(R_CONFIG_ACTIVE = "desktop")`.

### Running data.prep

Once the above steps have been completed, you should be able to [run run_pacta_data_preparation.R](https://github.com/RMI-PACTA/workflow.data.preparation/blob/main/run_pacta_data_preparation.R), either by sourcing it, e.g. `source("run_pacta_data_preparation.R")`, or by running it line-by-line (or select lines of it) interactively.

## Running locally with `docker-compose`

Running the workflow requires a file `.env` to exist in the root directory, that looks like...

```sh
# .env
HOST_INPUTS_PATH=/PATH/TO/inputs
HOST_OUTPUTS_PATH=/PATH/TO/YYYYQQ_pacta_analysis_inputs_YYYY-MM-DD/YYYYQQ
GITHUB_PAT=ghp_XXXXxxXxXXXxXxxX
R_CONFIG_ACTIVE=YYYYQQ
```

- `HOST_INPUTS_PATH` the local path to where the input files live.
  `docker-compose` volume mounts this directory and reads and writes files to it, so it requires appropriate permissions on the host filesystem.
  The pacta.data.preparation process requires input files that must exist in this directory and they must have filenames that match those specified in the [config.yml](config.yml) for the specified config.
  See ["Required Input Files"](#required-input-files) (below) for more information.
- `HOST_OUTPUTS_PATH` the local path to save the output files.
  `docker-compose` volume mounts this directory and writes files to it, so it requires appropriate permissions on the host filesystem.
- `GITHUB_PAT` valid GitHub PAT that grants access to the repos:
  - [RMI-PACTA/pacta.scenario.preparation](https://github.com/RMI-PACTA/pacta.scenario.preparation)
  - [RMI-PACTA/pacta.data.preparation](https://github.com/RMI-PACTA/pacta.data.preparation)
- `R_CONFIG_ACTIVE` the name of the config to use.
  The [config.yml](config.yml) file contains named configurations which define the settings used during PACTA data preparation.
  See top-level yaml names of [config.yml](config.yml) for valid options.

Run `docker-compose up` from the root directory, and docker will build the image (if necessary), and then run the data.prep process given the specified options in the .env file.

Use `docker-compose build --no-cache` to force a rebuild of the Docker image.

## Required Input Files

All required files must exist at `$HOST_INPUTS_PATH`, in a single directory (no subdirectories).

### Asset Impact Data

Files from Asset Impact provide production forecasts.
The required files are:

- masterdata_ownership e.g. "2022-08-15_rmi_masterdata_ownership_2021q4.csv"
- masterdata_debt e.g. "2023-01-13_rmi_masterdata_debt_2021q4.csv"
- ar_company_id__factset_entity_id e.g. "2022-08-17_rmi_ar_fs_id_bridge_2021q4.csv"

### FactSet Data

Files exported by [`{workflow.factset}`](https://github.com/RMI-PACTA/workflow.factset) provide financial data to tie to production data.
See the [`workflow.factset` README](https://github.com/RMI-PACTA/workflow.factset#exported-files) for more information on expected file format.

The required files are:

- factset_entity_financing_data.rds
- factset_entity_info.rds
- factset_financial_data.rds
- factset_fund_data.rds
- factset_isin_to_fund_table.rds
- factset_iss_emissions.rds
