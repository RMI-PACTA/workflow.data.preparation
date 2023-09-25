# workflow.data.preparation

Running the workflow requires a file `.env` to exist in the root directory, that looks like...

``` sh
HOST_INPUTS_PATH=/PATH/TO/AR_YYYYQQ
HOST_OUTPUTS_PATH=/PATH/TO/YYYYQQ_pacta_analysis_inputs_YYYY-MM-DD/YYYYQQ
GITHUB_PAT=ghp_XXXXxxXxXXXxXxxX
R_DATABASE_USER=xxxx@xxxxx
R_DATABASE_PASSWORD=xxxXxxXxxxxxxXXxxxxXXXXxx
R_CONFIG_ACTIVE=YYYYQQ
```

- `HOST_INPUTS_PATH` the local path to where the input files live. This directory will be mounted into the Docker image and it will read and write some files to this directory, so appropriate permissions in this directory will be required. The pacta.data.preparation process requires 3 input files that must exist in this directory and they must have filenames that match those specified in the [config.yml](config.yml) for the specified config:
  - masterdata_ownership e.g. "2022-08-15_rmi_masterdata_ownership_2021q4.csv"
  - masterdata_debt e.g. "2023-01-13_rmi_masterdata_debt_2021q4.csv"
  - ar_company_id__factset_entity_id e.g. "2022-08-17_rmi_ar_fs_id_bridge_2021q4.csv"
- `HOST_OUTPUTS_PATH` the local path to where the output files should be saved. This directory will be mounted into the Docker image and it will write some files to this directory, so appropriate permissions in this directory will be required.
- `GITHUB_PAT` valid GitHub PAT that grants access to the repos [RMI-PACTA/pacta.scenario.preparation](https://github.com/RMI-PACTA/pacta.scenario.preparation) and [RMI-PACTA/pacta.data.preparation](https://github.com/RMI-PACTA/pacta.data.preparation)
- `R_DATABASE_USER` username for the FactSet database
- `R_DATABASE_PASSWORD` password for the FactSet database
- `R_CONFIG_ACTIVE` the name of the config to be used. The [config.yml](config.yml) file contains multiple named configurations which can be named here to activate them. Current options are: `2021Q4`, `2021Q4_dev_vm`, `2021Q4_dev_local`, `2021Q4_prod_vm`, `2022Q2`

Run `docker-compose up` from the root directory, and docker will build the image (if necessary), and then run the data.prep process given the specified options in the .env file.

Use `docker-compose build --no-cache` to force a rebuild of the Docker image.

## Docker image for Azure Container Instance

`Dockerfile.azure` is intended to be built and run as an Azure Container Instance.

Please note that this Dockerfile is intended to be built using [buildkit](https://docs.docker.com/build/buildkit/), since it relies on passing secrets.

To build this image, create a file containing the _value_ of the GitHub PAT (with access to necessary repos), and build using buildkit:

```sh
# docker buildx build 
# is equivilent to
# DOCKER_BUILDKIT=1 docker build

# Note that path to secretfile must be an absolute path 
# or use $(pwd) if in working dir

docker buildx build \
  --secret id=gh_pat,src=/path/to/secretfile \
  --tag workflow.data.preparation_aci \
  -f Dockerfile.azure . 

```
