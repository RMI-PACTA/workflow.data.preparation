# Docker image for running on Azure Container Instances

## Build

Because the docker image includes private repos, you must include a GITHUB PAT in the file `ACI/github_pat.txt` in order for the following command to work.

```sh
# Note that path to secretfile must be an absolute path 
# or use $(pwd) if in working dir

# must build with buildkit
# run from repo root
docker build \
  --secret id=github_pat,src=$(pwd)/ACI/github_pat.txt \
  --progress=plain \
  --tag transitionmonitordockerregistry.azurecr.io/workflow.data.preparation_aci \
  -f ACI/Dockerfile.ACI . 

```

## Running on Azure VM

Due to data prep requiring >16 GB of memory (which is the current limit for standard ACI runners), `deploy-vm.sh` is a script that will initialize a virtual machine, mount the required file shares, and pull and run the docker container.
**The docker image must exist on the continer registry before running the script.**

```sh

# must set envvar RESOURCEGROUP and refer to an exisitng Azure RG.
# set inline
# RESOURCEGROUP="<myRG>" ./deploy-vm.sh
# or with export:
# export RESOURCEGROUP="<myRG>"

./deploy-vm.sh

```

**Please delete the VM when it's finished!**

### Inpsecting the VMs

You can connect to the VM if you first connect to the VPN (the deploy script does not create a public IP).

```sh

az ssh vm \
    --local-user azureuser \
    --name "data-prep-runner" \
    --prefer-private-ip \
    --resource-group "$RESOURCEGROUP"

```

### Watching progress

from an SSH session (above):

```sh

tmux new-session tail -n 10000 -f /var/log/cloud-init-output.log

```
