# Load config
library(rlog)

log_info("Loading config")
cfg <- config::get()

masterdata_path <- file.path(
  cfg[["raw_data_path"]],
  "AssetImpact",
  "Masterdata",
  cfg[["pacta_financial_timestamp"]]
)

masterdata_debt <- file.path(
  masterdata_path,
  cfg[["masterdata_debt_filename"]]
)

masterdata_ownership <- file.path(
  masterdata_path,
  cfg[["masterdata_ownership_filename"]]
)

ar_fs_bridge <- file.path(
  cfg[["raw_data_path"]],
  "AssetImpact",
  "FactSet_Bridge",
  cfg[["ar_company_id__factset_entity_id_filename"]]
)

files_to_copy <- c(
  masterdata_debt,
  masterdata_ownership,
  ar_fs_bridge
)

missing_files <- !file.exists(files_to_copy)
if (any(missing_files)) {
  log_error("The following files are missing:")
  log_error(files_to_copy[missing_files])
  stop("Please ensure the config points to extant files.")
}

if (!dir.exists(cfg[["data_prep_inputs_path"]])) {
  log_info("Creating data_prep_inputs_path")
  dir.create(cfg[["data_prep_inputs_path"]])
} else {
  log_warn("data_prep_inputs_path already exists")
}

if (!dir.exists(cfg[["data_prep_outputs_path"]])) {
  log_info("Creating data_prep_outputs_path")
  dir.create(cfg[["data_prep_outputs_path"]])
} else {
  log_warn("data_prep_outputs_path already exists")
}

log_info("Copying files")
for (x in files_to_copy) {
  destination <- file.path(cfg[["data_prep_inputs_path"]], basename(x))
  log_debug(sprintf("Copying %s to %s", x, destination))
  copy_success <- file.copy(
    from = x,
    to = destination,
    overwrite = FALSE
  )
  if (!copy_success) {
    log_error(sprintf("Failed to copy %s to %s", x, destination))
    stop("File copy error")
  }

  source_md5 <- digest::digest(
    object = x,
    algo = "md5",
    file = TRUE
  )
  destination_md5 <- digest::digest(
    object = destination,
    algo = "md5",
    file = TRUE
  )
  if (source_md5 != destination_md5) {
    log_error(sprintf("MD5 mismatch for %s", basename(x)))
    stop("MD5 mismatch.")
  }

}
log_info("Files copied.")
