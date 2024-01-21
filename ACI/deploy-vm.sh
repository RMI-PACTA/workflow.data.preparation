#! /bin/sh

# check that RESOURCEGROUP is set
if [ -z "$RESOURCEGROUP" ]; then
    echo "envvar RESOURCEGROUP is not set"
    exit 1
fi

SUBNETID=$(az network vnet subnet show --resource-group RMI-PROD-EU-VNET-RG --name Server --vnet-name RMI-PROD-EU-VNET --query id -o tsv)
MACHINENAME="data-prep-runner"
MACHINEIDENTITY="/subscriptions/feef729b-4584-44af-a0f9-4827075512f9/resourceGroups/pacta-experiments/providers/Microsoft.ManagedIdentity/userAssignedIdentities/ACR-test-01"
MACHINESIZE="Standard_E4-2as_v4"

if [ -n "$DEBUG" ]; then
    echo "MACHINEIDENTITY: $MACHINEIDENTITY"
    echo "MACHINENAME: $MACHINENAME"
    echo "MACHINESIZE: $MACHINESIZE"
    echo "RESOURCEGROUP: $RESOURCEGROUP"
    echo "SUBNETID: $SUBNETID"
fi

az vm create \
  --resource-group "$RESOURCEGROUP" \
  --name "$MACHINENAME" \
  --image Ubuntu2204 \
  --assign-identity "$MACHINEIDENTITY" \
  --admin-username azureuser \
  --subnet "$SUBNETID" \
  --public-ip-address "" \
  --size "$MACHINESIZE" \
  --generate-ssh-keys \
  --nic-delete-option delete \
  --os-disk-delete-option delete \
  --custom-data cloud-init.txt
