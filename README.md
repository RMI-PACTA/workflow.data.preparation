requires a file `.env` to exist in the root directory, that looks like...
``` sh
HOST_INPUTS_PATH=~/Desktop/inputs
HOST_OUTPUTS_PATH=~/Desktop/outputs
GITHUB_PAT=ghp_XXXXxxXxXXXxXxxX
R_DATABASE_USER=xxxx@xxxxx
R_DATABASE_PASSWORD=xxxXxxXxxxxxxXXxxxxXXXXxx
R_CONFIG_ACTIVE=2021Q4
```

- `HOST_INPUTS_PATH` the local path to where the input files live. This directory will be mounted into the Docker image and it will read and write some files to this directory, so appropriate permissions in this directory will be required. The pacta.data.preparation process requires 3 input files that must exist in this directory and they must have filenames that match those specified in the config.yml for the specified config.
- `HOST_OUTPUTS_PATH` the local path to where the output files should be saved. This directory will be mounted into the Docker image and it will write some files to this directory, so appropriate permissions in this directory will be required.
- `GITHUB_PAT` valid GitHub PAT that grants access to the repos [RMI-PACTA/pacta.scenario.preparation]() and [RMI-PACTA/pacta.data.preparation]()
- `R_DATABASE_USER` username for the FactSet database
- `R_DATABASE_PASSWORD` password for the FactSet database
- `R_CONFIG_ACTIVE` the name of the config to be used. The config.yml file contains multiple named configurations which can be named here to activate them. Current options are: `2021Q4`, `2021Q4_dev_vm`, `2021Q4_dev_local`, `2021Q4_prod_vm`, `2022Q2`



