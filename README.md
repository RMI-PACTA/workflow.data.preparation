<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# workflow.data.preparation

`workflow.data.preparation` orchestrates the PACTA data preparation process, combining production, financial, scenario, and currency data into a format suitable for use in a PACTA for investors analysis. Assuming that the computing resource being used has sufficient memory (which can be >16Gb depending on the inputs), storage space, and access to the necessary inputs, this is intended to work on a desktop or laptop using RStudio or run using the included [Dockerfile](https://github.com/RMI-PACTA/workflow.data.preparation/blob/main/Dockerfile) and [docker-compose.yml](https://github.com/RMI-PACTA/workflow.data.preparation/blob/main/docker-compose.yml).

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
HOST_FACTSET_EXTRACTED_PATH=/PATH/TO/factset-extracted
HOST_ASSET_IMPACT_PATH=/PATH/TO/asset-impact
HOST_SCENARIO_INPUTS_PATH=/PATH/TO/scenario-sources
HOST_OUTPUTS_PATH=/PATH/TO/YYYYQQ_pacta_analysis_inputs_YYYY-MM-DD/YYYYQQ
GITHUB_PAT=ghp_XXXXxxXxXXXxXxxX
R_CONFIG_ACTIVE=YYYYQQ
```

- `HOST_FACTSET_EXTRACTED_PATH` the local path to where the FactSet input files live.
  `docker-compose` volume mounts this directory and reads files from it, so it requires appropriate permissions on the host filesystem.
  The pacta.data.preparation process requires input files that must exist in this directory and they must have filenames that match those specified in the [config.yml](config.yml) for the specified config.
  See ["Required Input Files"](#required-input-files) (below) for more information.
- `HOST_ASSET_IMPACT_PATH` the local path to where the Asset Impact input files live.
  `docker-compose` volume mounts this directory and reads files from it, so it requires appropriate permissions on the host filesystem.
  The pacta.data.preparation process requires input files that must exist in this directory and they must have filenames that match those specified in the [config.yml](config.yml) for the specified config.
  See ["Required Input Files"](#required-input-files) (below) for more information.
- `HOST_SCENARIO_INPUTS_PATH` the local path to where the scenarios input files live.
  `docker-compose` volume mounts this directory and reads files from it, so it requires appropriate permissions on the host filesystem.
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

## Running Data Preparation interactively on Azure VM

*Instructions specific to the RMI-PACTA team's Azure instance are in Italics.*

0. **Prerequisites:**
    *These steps have been completed on the RMI Azure instance.*
    - Ensure a Virtual Network with a Gateway has been set up, permitting SSH (Port 22) access.
      Details of setting this up are out of scope for these instructions.
      Talk to your network coordinator for help.
    - Set up Storage Accounts containing the [required files](#required-input-files).
      While all the files can exist on a single file share, in a single storage account, the workflow can access different storage accounts, to allow for read-only access to raw data, to prevent accident manipulation of source data.
      The recommended structure (*used by RMI*) is:
      - Storage Account: `pactadatadev`: (read/write).
        Naming note: *RMI QAs datasets prior to moving them to PROD with [`workflow.pacta.data.qa`](https://github.com/RMI-PACTA/workflow.pacta.data.qa)*.
        - File Share `workflow-data-preparation-outputs`: Outputs from this workflow.
      - Storage Account: `pactarawdata` (read-only)
        - File Share `factset-extracted`: Outputs from [`workflow.factset`](https://github.com/RMI-PACTA/workflow.factset)
        - File Share `AssetImpact` Raw data files from [Asset Impact](https://asset-impact.gresb.com/)
    - (Optional, but recommended) Create a User Assigned Managed Identity.
      Alternately, after creating the VM with a system-managed identity, you can assign all appropriate permissions. ***RMI:** The `workflow-data-preparation` Identity exists with all the appropriate permissions.*
    - Grant Appropriate permissions to the Identity:
      - `pactadatadev`: "Reader and Data Access".
      - `pactarawdata`: "Reader and Data Access"
        Note that this gives read/write access the Storage Account via the Storage Account Key.
        To grant read-only access to the VM, use the `mount_afs` script without the `-w` flag, as shown below.

1. **Start a VM**
  While the machine can be deployed via the Portal (WebUI), for simplicity, the following code block is provided which ensures consistency:

    ```sh
    # The options here work with the RMI-PACTA team's Azure setup.
    # Change values for your own instance as needed.

    # Get Network details.
    VNET_RESOURCE_GROUP="RMI-PROD-EU-VNET-RG"
    VNET_NAME="RMI-PROD-EU-VNET"
    SUBNET_NAME="RMI-SP-PACTA-DEV-VNET"
    SUBNET_ID=$(az network vnet subnet show --resource-group $VNET_RESOURCE_GROUP --name $SUBNET_NAME --vnet-name $VNET_NAME --query id -o tsv)

    # Use the identity previously setup (see Prerequisites)
    MACHINEIDENTITY="/subscriptions/feef729b-4584-44af-a0f9-4827075512f9/resourceGroups/RMI-SP-PACTA-PROD/providers/Microsoft.ManagedIdentity/userAssignedIdentities/workflow-data-preparation"
    # This size has 2 vCPU, and 32GiB memory, recommended settings.
    MACHINE_SIZE="Standard_E4-2as_v4"
    # Using epoch to give machine a (probably) unique name
    MACHINE_NAME="dataprep-runner-$(date +%s)"
    # NOTE: Change this to your own RG as needed.
    VM_RESOURCE_GROUP="RMI-SP-PACTA-DEV"

    # **NOTE: Check these options prior to running**
    # Non-RMI users may choose to omit the --public-ip-address line for public SSH Access.

    az vm create \
      --admin-username azureuser \
      --assign-identity "$MACHINEIDENTITY" \
      --generate-ssh-keys  \
      --image Ubuntu2204 \
      --name "$MACHINE_NAME" \
      --nic-delete-option delete \
      --os-disk-delete-option delete \
      --public-ip-address "" \
      --resource-group "$VM_RESOURCE_GROUP" \
      --size "$MACHINE_SIZE" \
      --subnet "$SUBNET_ID"

    ```

    If this command successfully runs, it will output a JSON block describing the resource (VM) created.

2. **Connect to the Network.** (Optional)
  ***RMI:** Connecting to the VPN will enable SSH access.*
  Connect to the Virtual Network specified above, as the comand above does not create a Public IP Address.
  Details for this are out of scope for these instructions.
  Contact your network coordinator for help.

3. **Connect to the newly created VM via SSH.**

    ```sh
    # This connects to the VM created above via SSH.
    # See above block for envvars referenced here.

    az ssh vm \
        --local-user azureuser \
        --name "$MACHINE_NAME" \
        --prefer-private-ip \
        --resource-group "$VM_RESOURCE_GROUP"

    ```

4. **Connect the VM to required resources**
    Clone this repo, install the `az` cli utility, and mount the appropriate Azure File Shares.

    ```sh
    # Clone this repo through https to avoid need for an SSH key
    git clone https://github.com/RMI-PACTA/workflow.data.preparation.git ~/workflow.data.preparation

    # Install az cli
    sudo apt update
    # See https://aka.ms/installcli for alternate instructions
    curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash

    # Login to azure with assigned identity
    az login --identity

    # Use script from this repo to connect to file shares
    ~/workflow.data.preparation/scripts/mount_afs.sh -r "RMI-SP-PACTA-PROD" -a "pactarawdata" -f "factset-extracted" -m "/mnt/factset-extracted"
    ~/workflow.data.preparation/scripts/mount_afs.sh -r "RMI-SP-PACTA-PROD" -a "pactarawdata" -f "asset-impact" -m "/mnt/asset-impact"

    # Note the outputs directory has the -w flag, meaning write permissions are enabled.
    ~/workflow.data.preparation/scripts/mount_afs.sh -r "RMI-SP-PACTA-DEV" -a "pactadatadev" -f "workflow-data-preparation-outputs" -m "/mnt/workflow-data-preparation-outputs" -w

    ```

5. **Install Docker**

    ```sh
    # install docker
    sudo apt -y install \
        docker-compose \
        docker.io

    # Allow azureuser to run docker without sudo
    sudo usermod -aG docker azureuser
    ```

    At this point, you need to log out of the shell to reevaluate group memberships (add the `docker` group to `azureuser`).
    You can log back in with the `az ssh` command from step 3.
    When you are back into the shell, you can run `docker run --rm hello-world` to confirm that docker is working correctly, and you are able to run as a non-root user.

6. **Prepare `.env` file**
  The `ubuntu2204` image used for the VM includes both `vim` and `nano`.
  Create a `.env` file in the `workflow.data.preparation` directory, according to the instructions in the [running locally](running-locally-with-docker-compose) section of this file.

7. **Build Docker image**
    The cloned git repo in the home directory, and mounted directories should sill be in place after logging in again.
    Additionally, `azureuser` should be part of the `docker` group.
    you can confirm this with:

    ```sh
    groups
    ls ~
    ls /mnt
    ```

    With that in place, you are ready to build the `workflow.data.preparation` docker image.
    **To ensure that a dropped network connection does not kill the process, you should run this in `tmux`.**
    
    ```sh
    # navigate to the workflow.data.preparation repo
    cd ~/workflow.data.preparation

    tmux

    docker-compose build

    docker-compose up

    ```

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

### Scenarios Data

Files exported by [`{workflow.scenario.preparation}`](https://github.com/RMI-PACTA/workflow.scenario.preparation) provide scenario data to be combined with the ABCD data.
See the [`workflow.scenario.preparation` README](https://github.com/RMI-PACTA/workflow.scenario.preparation) for more information on expected file format.

The required files are:

- factset_entity_financing_data.rds
- factset_entity_info.rds
- factset_financial_data.rds
- factset_fund_data.rds
- factset_isin_to_fund_table.rds
- factset_iss_emissions.rds
- factset_issue_code_bridge.rds
- factset_industry_map_bridge.rds
- factset_manual_pacta_sector_override.rds
- geco_2022.csv
