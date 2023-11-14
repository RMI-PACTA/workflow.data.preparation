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

`Dockerfile.ACI` is intended to be built and run as an Azure Container Instance.

Please note that this Dockerfile is intended to be built using [buildkit](https://docs.docker.com/build/buildkit/), since it relies on passing secrets.

To build this image, create a file containing the _value_ of the GitHub PAT (with access to necessary repos), and build using buildkit.

Up-to-date installations of docker on MacOS and Windows likely already have buildkit enabled.
It is possible to check your docker configuration, and check for `buildkit: true`.
If it is not enabled on your system, then you can either `export DOCKER_BUILDKIT=1 docker build`, or replace the `docker build` commands below with `docker buildx build` (either works).

If your installed docker engine (found by running `docker version`) is > 20.10.0, then the secret can read from your local `GITHUB_PAT` envvar (must be `export`ed).

```sh

# must be built with buildkit
# run from repo root
docker build \
  --secret id=github_pat,env=GITHUB_PAT \
  --progress=plain \
  --tag workflow.data.preparation_aci \
  -f ACI/Dockerfile.ACI . 

```

For older docker versions that support buildkit, you can write the _value_ of the token to a file, and specifiy the absolute path to that file instead.

```sh
# Note that path to secretfile must be an absolute path 
# or use $(pwd) if in working dir

# must be built with buildkit
# run from repo root
docker build \
  --secret id=github_pat,src=$(pwd)/secretfile \
  --progress=plain \
  --tag workflow.data.preparation_aci \
  -f ACI/Dockerfile.ACI . 

```

The image then needs to be pushed to a registry, for use with `azure-deploy.json`

### Deploy process

#### Prerequisites

[Containers ARM Schema](https://learn.microsoft.com/en-us/azure/templates/microsoft.containerinstance/containergroups?pivots=deployment-language-arm-template#resource-format)

[secrets](https://learn.microsoft.com/en-us/azure/container-apps/manage-secrets?tabs=azure-portal)

- Azure Key Vault: the deploy process reads secrets from an Azure Key vault. The essential values refenced in the ARM template are:
  - Storage Account Key for raw data storage (`rawdata-storageAccountKey`)
  - Storage Account Key for "input" data storage (`dataprepinputs-storageAccountKey`)
  - Storage Account Key for "output" data storage (`dataprepoutputs-storageAccountKey`)
  - Username for FactSet database (`factset-database-user`)
  - Password for FactSet database (`factset-database-password`)
Note that the Storage account keys are passed as parameters via `azure-deploy.parameters.json`, while the database credentials are used by the application itself, are are __freely readable__ if accessing the container (via `exec`, for example).

To get the storage keys:

```sh
# replace these values with storage account name and resource group appropriate to your deployment
ACI_PERS_STORAGE_ACCOUNT_NAME="pactadata"
ACI_PERS_RESOURCE_GROUP="pacta-data"

STORAGE_KEY=$(az storage account keys list --resource-group "$ACI_PERS_RESOURCE_GROUP" --account-name "$ACI_PERS_STORAGE_ACCOUNT_NAME" --query "[0].value" --output tsv)
echo "$STORAGE_KEY"
```

#### Deploy

```sh
# change this value as needed.
RESOURCEGROUP="myResourceGroup"

# run from repo root
az deployment group create --resource-group "$RESOURCEGROUP" --template-file ACI/azure-deploy.json --parameters @ACI/azure-deploy.parameters.json

```

### Helpful tips

To attach to the container and execute commands interactively (for debugging)

```sh

az container exec --resource-group "$RESOURCEGROUP" --name "<Deployment Group Name>" --container-name "data-prep" --exec-command "/bin/bash"

```

To start a long-running process (to allow for attaching and debugging), add this to `properties` for the container:

```json
  "command": [
    "tail", "-f", "/dev/null"
  ]
```
