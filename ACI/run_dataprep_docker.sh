#! /bin/sh

az login --identity

mount_afs -v -r pacta-data -a pactarawdata -f factset-extracted -m /mnt/factset-extracted
mount_afs -v -r pacta-data -a pactarawdata -f rawdata -m /mnt/rawdata
mount_afs -v -r pacta-data -a pactadata -f data-prep-inputs -m /mnt/dataprep_inputs
mount_afs -v -r pacta-data -a pactadata -f data-prep-outputs -m /mnt/outputs

az acr login --name transitionmonitordockerregistry

DOCKERIMAGE="transitionmonitordockerregistry.azurecr.io/workflow.data.preparation_aci"
docker pull $DOCKERIMAGE

docker run \
  -i -t --rm \
  --env DEPLOY_START_TIME=$(date -u +%Y%m%dT%H%M%SZ) \
  --env LOG_LEVEL=TRACE \
  --env R_CONFIG_ACTIVE=2022Q4_CICD \
  --mount type=bind,source=/mnt/dataprep_inputs,target=/mnt/dataprep_inputs \
  --mount type=bind,source=/mnt/factset-extracted,target=/mnt/factset-extracted,readonly \
  --mount type=bind,source=/mnt/outputs,target=/mnt/outputs \
  --mount type=bind,source=/mnt/rawdata,target=/mnt/rawdata,readonly \
  $DOCKERIMAGE
