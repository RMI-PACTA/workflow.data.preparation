logger::log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))
logger::log_formatter(logger::formatter_glue)

# Check value and format of $DEPLOY_START_TIME
deploy_start_time <- Sys.getenv("DEPLOY_START_TIME", "")
time_pattern <- "^[[:digit:]]{8}T[[:digit:]]{6}Z$"
if (grepl(x = deploy_start_time, pattern = time_pattern)) {
  logger::log_debug("DEPLOY_START_TIME: ", deploy_start_time)
  logger::log_trace("DEPLOY_START_TIME format is correct. ({time_pattern})")
} else if (nchar(deploy_start_time) == 0L) {
  logger::log_error(
    "Environment variable $DEPLOY_START_TIME not set or is empty"
  )
  stop("Environment variable DEPLOY_START_TIME not set")
} else {
  logger::log_warn("
    Environment variable $DEPLOY_START_TIME is not in the expected format. \\
    Expected format: '{time_pattern}'. \\
    Actual value: '{deploy_start_time}'. \\
    This variable is used to ensure consistency in accessing datasets. \\
    ")
}

logger::log_info("Loading config: ", Sys.getenv("R_CONFIG_ACTIVE", "default"))
cfg <- config::get()
logger::log_trace("Config loaded.")

masterdata_path <- file.path(
  cfg[["raw_data_path"]],
  "AssetImpact",
  "Masterdata",
  cfg[["pacta_financial_timestamp"]]
)
logger::log_trace("masterdata_path: ", masterdata_path)

masterdata_debt <- file.path(
  masterdata_path,
  cfg[["masterdata_debt_filename"]]
)
logger::log_trace("masterdata_debt file: ", masterdata_debt)

masterdata_ownership <- file.path(
  masterdata_path,
  cfg[["masterdata_ownership_filename"]]
)
logger::log_trace("masterdata_ownership file: ", masterdata_ownership)

ar_fs_bridge <- file.path(
  cfg[["raw_data_path"]],
  "AssetImpact",
  "FactSet_Bridge",
  cfg[["ar_company_id__factset_entity_id_filename"]]
)
logger::log_trace("ar_fs_bridge file: ", ar_fs_bridge)

factset_files <- list.files(
  path = file.path(
    cfg[["factset-extracted_path"]],
    cfg[["factset_dataset"]]
  ),
  include.dirs = FALSE,
  full.names = TRUE
)
logger::log_trace("factset_file: {factset_files}")

files_to_copy <- c(
  masterdata_debt,
  masterdata_ownership,
  ar_fs_bridge,
  factset_files
)

missing_files <- !file.exists(files_to_copy)
if (any(missing_files)) {
  logger::log_error("The following files are missing:")
  logger::log_error("{files_to_copy[missing_files]}")
  stop("Please ensure the config points to existing files.")
}

if (dir.exists(cfg[["data_prep_inputs_path"]])) {
  logger::log_warn("data_prep_inputs_path already exists")
} else {
  logger::log_debug(
    "Creating data_prep_inputs_path: {cfg[['data_prep_inputs_path']]}}"
  )
  dir.create(cfg[["data_prep_inputs_path"]])
}
logger::log_info(
  "copying files to data_prep_inputs_path: {cfg[['data_prep_inputs_path']]}}"
)

logger::log_info("Copying files")
for (source_file in files_to_copy) {

  destination_file <- file.path(
    cfg[["data_prep_inputs_path"]],
    basename(source_file)
  )
  if (file.exists(destination_file)) {
    logger::log_warn(
      "Destination file already exists: {destination_file}."
    )
  }
  logger::log_debug("Copying: {source_file} -> {destination_file}")
  copy_success <- file.copy(
    from = source_file,
    to = destination_file,
    overwrite = FALSE
  )
  if (copy_success) {
    logger::log_trace("Copy success")
  } else {
    logger::log_error("Failed to copy {source_file} to {destination_file}")
    stop("File copy error")
  }

  source_md5 <- digest::digest(
    object = source_file,
    algo = "md5",
    file = TRUE
  )
  destination_md5 <- digest::digest(
    object = destination_file,
    algo = "md5",
    file = TRUE
  )
  if (identical(source_md5, destination_md5)) {
    logger::log_trace("MD5 match: {unique(source_md5, destination_md5)}")
  } else {
    logger::log_error(sprintf("MD5 mismatch for %s", basename(source_file)))
    logger::log_error("Source MD5: {source_md5} {source_file}")
    logger::log_error("Destination MD5: {destination_md5} {destination_file}")
    stop("MD5 mismatch.")
  }

}
logger::log_info("Done copying files")
