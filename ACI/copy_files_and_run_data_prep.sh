#! /bin/sh
set -e

# check memory available
free -m | cat

inputs_dir="/mnt/dataprep_inputs"

# copy raw data, then run normal data prep script
Rscript /workflow.data.preparation/copy_raw_data.R 2>&1 | \
  tee "$inputs_dir/$DEPLOY_START_TIME-copy.log"

Rscript /workflow.data.preparation/run_pacta_data_preparation.R 2>&1 | \
  tee "$inputs_dir/$DEPLOY_START_TIME-prep.log"

exit 0
