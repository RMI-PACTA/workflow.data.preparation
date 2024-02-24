#! /bin/sh

#  mount an Azure File Share at a given location.
#  Requires az cli to be installed and logged in.

usage() {
    echo "Usage: mount_afs.sh [-h] [-v] -r <resource group> -a <storage account name> -f <file share name> -m <mount point>"
    echo "  -h: help (this message)"
    echo "  -v: verbose"
    echo "  -w: Allow write access to the file share (default is read-only)"
    echo "  -r: resource group (Required)"
    echo "  -a: storage account name (Required)"
    echo "  -f: file share name (Required)"
    echo "  -m: mount point (Required)"
    echo "  -?: help"
    exit 1
}

while getopts "h?vwr:a:f:m:" opt; do
    case "$opt" in
    h|\?)
        usage
        ;;
    v)  VERBOSE=1
        ;;
    w)  ALLOW_WRITE=1
        ;;
    r)  RESOURCEGROUP=$OPTARG
        ;;
    a)  STORAGEACCOUNTNAME=$OPTARG
        ;;
    f)  FILESHARENAME=$OPTARG
        ;;
    m)  MOUNTPOINT=$OPTARG
        ;;
    *)
        usage
        ;;
    esac
done

missing_opts=0
if [ -z "$RESOURCEGROUP" ]; then
    echo "ERROR: Resource group is required"
    missing_opts=1
fi

if [ -z "$STORAGEACCOUNTNAME" ]; then
    echo "ERROR: Storage account name is required"
    missing_opts=1
fi

if [ -z "$FILESHARENAME" ]; then
    echo "ERROR: File share name is required"
    missing_opts=1
fi

if [ -z "$MOUNTPOINT" ]; then
    echo "ERROR: Mount point is required"
    missing_opts=1
fi

if [ $missing_opts -eq 1 ]; then
    usage
fi

if [ -n "$VERBOSE" ]; then
    echo "RESOURCEGROUP: $RESOURCEGROUP"
    echo "STORAGEACCOUNTNAME: $STORAGEACCOUNTNAME"
    echo "FILESHARENAME: $FILESHARENAME"
    echo "MOUNTPOINT: $MOUNTPOINT"
fi

# This command assumes you have logged in with az login

if [ -n "$VERBOSE" ]; then
    echo "Getting https endpoint for storage account $STORAGEACCOUNTNAME"
fi

httpEndpoint=$(az storage account show \
  --resource-group "$RESOURCEGROUP" \
  --name "$STORAGEACCOUNTNAME" \
  --query "primaryEndpoints.file" --output tsv | tr -d '"')
smbPath=$(echo "$httpEndpoint" | cut -c7-${#httpEndpoint})$FILESHARENAME
fileHost=$(echo "$httpEndpoint" | cut -c7-${#httpEndpoint}| tr -d "/")
nc -zvw3 "$fileHost" 445

if [ -n "$VERBOSE" ]; then
    echo "httpEndpoint: $httpEndpoint"
    echo "smbPath: $smbPath"
    echo "fileHost: $fileHost"
fi

if [ -n "$VERBOSE" ]; then
    echo "Getting storage account key"
fi
storageAccountKey=$(az storage account keys list \
  --resource-group "$RESOURCEGROUP" \
  --account-name "$STORAGEACCOUNTNAME" \
  --query "[0].value" --output tsv | tr -d '"')

if [ -n "$VERBOSE" ]; then
    echo "Creating mount path: $MOUNTPOINT"
fi
sudo mkdir -p "$MOUNTPOINT"

if [ -n "$VERBOSE" ]; then
    echo "Mounting $smbPath to $MOUNTPOINT"
fi

if [ -n "$ALLOW_WRITE" ]; then
  permissions="file_mode=0777,dir_mode=0777"
else
  permissions="file_mode=0555,dir_mode=0555"
fi

sudo mount -t cifs "$smbPath" "$MOUNTPOINT" -o username="$STORAGEACCOUNTNAME",password="$storageAccountKey",serverino,nosharesock,actimeo=30,nobrl,"$permissions",vers=3.1.1
