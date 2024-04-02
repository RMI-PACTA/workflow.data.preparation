logger::log_threshold(Sys.getenv("LOG_LEVEL", ifelse(interactive(), "FATAL", "INFO")))
logger::log_formatter(logger::formatter_glue)

# necessary packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(pacta.data.preparation)
  library(pacta.data.scraping)
  library(pacta.scenario.preparation)
  library(DBI)
  library(dplyr)
  library(readr)
  library(rlang)
  library(RSQLite)
  library(tidyr)
})


# config -----------------------------------------------------------------------

if (Sys.getenv("R_CONFIG_ACTIVE") != "desktop") { readRenviron(".env") }

config_name <- Sys.getenv("R_CONFIG_ACTIVE")
raw_config <-
  config::get(
    file = "config.yml",
    config = config_name,
    use_parent = FALSE
  )

config <- raw_config

expected_keys <- c(
  "asset_impact_data_path",
  "factset_data_path",
  "data_prep_outputs_path",
  "preflight_data_path",
  "masterdata_ownership_filename",
  "masterdata_debt_filename",
  "ar_company_id__factset_entity_id_filename",
  "factset_financial_data_filename",
  "factset_entity_info_filename",
  "factset_entity_financing_data_filename",
  "factset_fund_data_filename",
  "factset_isin_to_fund_table_filename",
  "factset_iss_emissions_data_filename",
  "factset_issue_code_bridge_filename",
  "factset_industry_map_bridge_filename",
  "factset_manual_pacta_sector_override_filename",
  "update_currencies",
  "export_sqlite_files",
  "export_archives",
  "imf_quarter_timestamp",
  "pacta_financial_timestamp",
  "market_share_target_reference_year",
  "time_horizon",
  "scenario_sources_list",
  "sector_list",
  "other_sector_list",
  "zero_emission_factor_techs",
  "green_techs",
  "scenario_raw_data_to_include",
  "tech_exclude",
  "scenario_geographies_list",
  "global_aggregate_scenario_sources_list",
  "global_aggregate_sector_list"
)
stopifnot(all(expected_keys %in% names(config)))

# create timestamped output directory
system_timestamp <- format(
  Sys.time(),
  format = "%Y%m%dT%H%M%SZ",
  tz = "UTC"
)
config[["data_prep_outputs_path"]] <- file.path(
  config[["data_prep_outputs_path"]],
  paste(config[["pacta_financial_timestamp"]], system_timestamp, sep = "_")
)

if (dir.exists(config[["data_prep_outputs_path"]])) {
  logger::log_warn("POTENTIAL DATA LOSS: Output directory already exists, and files may be overwritten ({config[[\"data_prep_outputs_path\"]]}).")
  warning("Output directory exists. Files may be overwritten.")
} else {
  logger::log_trace("Creating output directory: \"{config[[\"data_prep_outputs_path\"]]}\"")
  dir.create(config[["data_prep_outputs_path"]], recursive = TRUE)
}

# input filepaths --------------------------------------------------------------

masterdata_ownership_path <-
  file.path(config[["asset_impact_data_path"]], config[["masterdata_ownership_filename"]])
masterdata_debt_path <-
  file.path(config[["asset_impact_data_path"]], config[["masterdata_debt_filename"]])
ar_company_id__factset_entity_id_path <-
  file.path(config[["asset_impact_data_path"]], config[["ar_company_id__factset_entity_id_filename"]])

factset_financial_data_path <-
  file.path(config[["factset_data_path"]], config[["factset_financial_data_filename"]])
factset_entity_info_path <-
  file.path(config[["factset_data_path"]], config[["factset_entity_info_filename"]])
factset_entity_financing_data_path <-
  file.path(config[["factset_data_path"]], config[["factset_entity_financing_data_filename"]])
factset_fund_data_path <-
  file.path(config[["factset_data_path"]], config[["factset_fund_data_filename"]])
factset_isin_to_fund_table_path <-
  file.path(config[["factset_data_path"]], config[["factset_isin_to_fund_table_filename"]])
factset_iss_emissions_data_path <-
  file.path(config[["factset_data_path"]], config[["factset_iss_emissions_data_filename"]])
factset_issue_code_bridge_path <-
  file.path(config[["factset_data_path"]], config[["factset_issue_code_bridge_filename"]])
factset_industry_map_bridge_path <-
  file.path(config[["factset_data_path"]], config[["factset_industry_map_bridge_filename"]])
factset_manual_pacta_sector_override_path <-
  file.path(config[["factset_data_path"]], config[["factset_manual_pacta_sector_override_filename"]])


# pre-flight filepaths ---------------------------------------------------------

if (config[["preflight_data_path"]] == "") {
  config[["preflight_data_path"]] <- config[["data_prep_outputs_path"]]
}

currencies_preflight_data_path <- file.path(config[["preflight_data_path"]], "currencies.rds")
currencies_data_path <- file.path(config[["data_prep_outputs_path"]], "currencies.rds")
index_regions_data_path <- file.path(config[["data_prep_outputs_path"]], "index_regions.rds")

index_regions_preflight_data_path <- file.path(config[["preflight_data_path"]], "index_regions.rds")

# computed options -------------------------------------------------------------

relevant_years <- sort(
  unique(
    config[["market_share_target_reference_year"]]:(config[["market_share_target_reference_year"]] + config[["time_horizon"]])
  )
)
logger::log_info("Full time horizon set to: {paste0(relevant_years, collapse = ', ')}.")

scenario_raw_data_to_include <- lapply(config[["scenario_raw_data_to_include"]], get, envir = asNamespace("pacta.scenario.preparation"))

factset_timestamp <-
  unique(sub("_factset_.*[.]rds$", "", c(
    config[["factset_financial_data_filename"]],
    config[["factset_entity_info_filename"]],
    config[["factset_entity_financing_data_filename"]],
    config[["factset_fund_data_filename"]],
    config[["factset_isin_to_fund_table_filename"]],
    config[["factset_iss_emissions_data_filename"]],
    config[["factset_issue_code_bridge_filename"]],
    config[["factset_industry_map_bridge_filename"]],
    config[["factset_manual_pacta_sector_override_filename"]]
  )))


# check that everything is ready to go -----------------------------------------

input_filepaths <- c(
  masterdata_ownership_path = masterdata_ownership_path,
  masterdata_debt_path = masterdata_debt_path,
  ar_company_id__factset_entity_id_path = ar_company_id__factset_entity_id_path,
  factset_financial_data_path = factset_financial_data_path,
  factset_entity_info_path = factset_entity_info_path,
  factset_entity_financing_data_path = factset_entity_financing_data_path,
  factset_fund_data_path = factset_fund_data_path,
  factset_isin_to_fund_table_path = factset_isin_to_fund_table_path,
  factset_iss_emissions_data_path = factset_iss_emissions_data_path,
  factset_issue_code_bridge_path = factset_issue_code_bridge_path,
  factset_industry_map_bridge_path = factset_industry_map_bridge_path,
  factset_manual_pacta_sector_override_path = factset_manual_pacta_sector_override_path
)

if (!config[["update_currencies"]]) {
  input_filepaths <- c(
    input_filepaths,
    currencies_preflight_data_path = currencies_preflight_data_path
  )
}

missing_input_files <- input_filepaths[!file.exists(input_filepaths)]

if (length(missing_input_files) > 0L) {
  logger::log_error("Input file cannot be found: {names(missing_input_files)} ({missing_input_files}).")
  stop("Input files are missing: ", toString(missing_input_files))
}

# pre-flight -------------------------------------------------------------------

logger::log_info("Fetching pre-flight data.")

if (config[["update_currencies"]]) {
  logger::log_info("Fetching currency data.")
  input_filepaths <- c(
    input_filepaths,
    currencies_preflight_data_path = currencies_preflight_data_path
  )
  currencies <- pacta.data.scraping::get_currency_exchange_rates(
    quarter = config[["imf_quarter_timestamp"]],
    max_seconds = 600L
  )
  saveRDS(currencies, currencies_preflight_data_path)
} else {
  logger::log_info("Using pre-existing currency data.")
  # This requires the preflight path to be defined in the config
  currencies <- readRDS(currencies_preflight_data_path)
}

logger::log_info("Scraping index regions.")
input_filepaths <- c(
  input_filepaths,
  index_regions_preflight_data_path = index_regions_preflight_data_path
)
index_regions <- pacta.data.scraping::get_index_regions()
saveRDS(index_regions, index_regions_preflight_data_path)

logger::log_info("Fetching pre-flight data done.")


# intermediary objects ---------------------------------------------------------

factset_issue_code_bridge <-
  readRDS(factset_issue_code_bridge_path) %>%
  select(issue_type_code, asset_type) %>%
  mutate(
    asset_type = case_when(
      .data$asset_type == "Listed Equity" ~ "Equity",
      .data$asset_type == "Corporate Bond" ~ "Bonds",
      .data$asset_type == "Fund" ~ "Funds",
      .data$asset_type == "Other" ~ "Others",
      TRUE ~ "Others"
    )
  )

factset_industry_map_bridge <-
  readRDS(factset_industry_map_bridge_path)

factset_manual_pacta_sector_override <-
  readRDS(factset_manual_pacta_sector_override_path)

logger::log_info("Preparing scenario data.")

scenario_regions <- pacta.scenario.preparation::scenario_regions

# scenario values will be linearly interpolated for each group below
interpolation_groups <- c(
  "source",
  "scenario",
  "sector",
  "technology",
  "scenario_geography",
  "indicator",
  "units"
)

scenario_raw <-
  bind_rows(scenario_raw_data_to_include) %>%
  pacta.scenario.preparation::interpolate_yearly(!!!rlang::syms(interpolation_groups)) %>%
  filter(.data$year >= .env$config[["market_share_target_reference_year"]]) %>%
  pacta.scenario.preparation::add_market_share_columns(reference_year = config[["market_share_target_reference_year"]]) %>%
  pacta.scenario.preparation::format_p4i(config[["green_techs"]])

# filter for relevant scenario data
scenarios_long <-
  scenario_raw %>%
  inner_join(
    pacta.scenario.preparation::scenario_source_pacta_geography_bridge,
    by = c(
      scenario_source = "source",
      scenario_geography = "scenario_geography_source"
      )
    ) %>%
  select(-"scenario_geography") %>%
  rename(scenario_geography = "scenario_geography_pacta") %>%
  filter(
    .data$scenario_source %in% .env$config[["scenario_sources_list"]],
    .data$ald_sector %in% c(.env$config[["sector_list"]], .env$config[["other_sector_list"]]),
    .data$scenario_geography %in% unique(.env$scenario_regions$scenario_geography),
    .data$year %in% unique(
      c(.env$relevant_years, .env$config[["market_share_target_reference_year"]] + 10)
    )
  )

logger::log_info("Scenario data prepared.")


# currency data output ---------------------------------------------------------

logger::log_info("Saving file: \"currencies.rds\".")
saveRDS(currencies, currencies_data_path)


# index_regions output ---------------------------------------------------------

logger::log_info("Saving file: \"index_regions.rds\".")
saveRDS(index_regions, index_regions_data_path)


# financial data output --------------------------------------------------------

logger::log_info("Preparing financial data.")

# read raw FactSet financial data, filter to unique rows, merge AR company_id,
# merge PACTA sectors from AR data
logger::log_info("Formatting and saving file: \"financial_data.rds\".")
readRDS(factset_financial_data_path) %>%
  pacta.data.preparation::prepare_financial_data(factset_issue_code_bridge) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "financial_data.rds"))
invisible(gc())

logger::log_info("Formatting and saving file: \"entity_financing.rds\".")
readRDS(factset_entity_financing_data_path) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "entity_financing.rds"))
invisible(gc())

logger::log_info("Formatting and saving file: \"entity_info.rds\".")
factset_entity_id__ar_company_id <-
  readr::read_csv(ar_company_id__factset_entity_id_path, col_types = "c") %>%
  pacta.data.preparation::prepare_factset_entity_id__ar_company_id()

readRDS(factset_entity_info_path) %>%
  pacta.data.preparation::prepare_entity_info(
    factset_entity_id__ar_company_id,
    factset_industry_map_bridge,
    factset_manual_pacta_sector_override
  ) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "entity_info.rds"))
invisible(gc())

logger::log_info("Financial data prepared.")


# ABCD data output -------------------------------------------------------------

logger::log_info("Preparing ABCD.")

entity_info <- readRDS(file.path(config[["data_prep_outputs_path"]], "entity_info.rds"))

ar_company_id__country_of_domicile <-
  pacta.data.preparation::prepare_ar_company_id__country_of_domicile(entity_info)

ar_company_id__credit_parent_ar_company_id <-
  pacta.data.preparation::prepare_ar_company_id__credit_parent_ar_company_id(entity_info)

rm(entity_info)
invisible(gc())


logger::log_info(
  "Formatting and saving file: \"masterdata_ownership_datastore.rds\"."
)
readr::read_csv(masterdata_ownership_path, na = "", show_col_types = FALSE) %>%
  pacta.data.preparation::prepare_masterdata(
    ar_company_id__country_of_domicile,
    config[["pacta_financial_timestamp"]],
    config[["zero_emission_factor_techs"]]
  ) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "masterdata_ownership_datastore.rds"))
invisible(gc())


logger::log_info(
  "Formatting and saving file: \"masterdata_debt_datastore.rds\"."
)

masterdata_debt <- readr::read_csv(masterdata_debt_path, na = "", show_col_types = FALSE)

company_id__creditor_company_id <-
  pacta.data.preparation::prepare_company_id__creditor_company_id(masterdata_debt)

masterdata_debt %>%
  pacta.data.preparation::prepare_masterdata(
    ar_company_id__country_of_domicile,
    config[["pacta_financial_timestamp"]],
    config[["zero_emission_factor_techs"]]
  ) %>%
  left_join(company_id__creditor_company_id, by = c(id = "company_id")) %>%
  left_join(ar_company_id__credit_parent_ar_company_id, by = c(id = "ar_company_id")) %>%
  mutate(id = if_else(!is.na(.data$credit_parent_ar_company_id), .data$credit_parent_ar_company_id, .data$id)) %>%
  mutate(id = if_else(!is.na(.data$creditor_company_id), .data$creditor_company_id, .data$id)) %>%
  mutate(id_name = "credit_parent_ar_company_id") %>%
  group_by(
    .data$id, .data$id_name, .data$ald_sector, .data$ald_location,
    .data$technology, .data$year, .data$country_of_domicile,
    .data$ald_production_unit, .data$ald_emissions_factor_unit,
  ) %>%
  summarise(
    ald_emissions_factor = stats::weighted.mean(.data$ald_emissions_factor, .data$ald_production, na.rm = TRUE),
    ald_production = sum(.data$ald_production, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "masterdata_debt_datastore.rds"))
invisible(gc())

rm(masterdata_debt)
rm(company_id__creditor_company_id)
invisible(gc())

rm(ar_company_id__country_of_domicile)
rm(ar_company_id__credit_parent_ar_company_id)
invisible(gc())

logger::log_info("ABCD prepared.")


# abcd_flags -------------------------------------------------------------------

logger::log_info("Preparing ABCD flags.")
financial_data <- readRDS(file.path(config[["data_prep_outputs_path"]], "financial_data.rds"))

factset_entity_id__ar_company_id <-
  readr::read_csv(ar_company_id__factset_entity_id_path, col_types = "c") %>%
  pacta.data.preparation::prepare_factset_entity_id__ar_company_id()

entity_info <- readRDS(file.path(config[["data_prep_outputs_path"]], "entity_info.rds"))

factset_entity_id__security_mapped_sector <-
  pacta.data.preparation::prepare_factset_entity_id__security_mapped_sector(entity_info)

factset_entity_id__credit_parent_id <-
  pacta.data.preparation::prepare_factset_entity_id__credit_parent_id(entity_info)

rm(entity_info)
invisible(gc())


logger::log_info("Formatting and saving file: \"abcd_flags_equity.rds\".")

ar_company_id__sectors_with_assets__ownership <-
  readRDS(file.path(config[["data_prep_outputs_path"]], "masterdata_ownership_datastore.rds")) %>%
  pacta.data.preparation::prepare_ar_company_id__sectors_with_assets__ownership(relevant_years)

pacta.data.preparation::prepare_abcd_flags_equity(
  financial_data,
  factset_entity_id__ar_company_id,
  factset_entity_id__security_mapped_sector,
  ar_company_id__sectors_with_assets__ownership
) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "abcd_flags_equity.rds"))

rm(ar_company_id__sectors_with_assets__ownership)
invisible(gc())


logger::log_info("Formatting and saving file: \"abcd_flags_bonds.rds\".")

ar_company_id__sectors_with_assets__debt <-
  readRDS(file.path(config[["data_prep_outputs_path"]], "masterdata_debt_datastore.rds")) %>%
  pacta.data.preparation::prepare_ar_company_id__sectors_with_assets__debt(relevant_years)

pacta.data.preparation::prepare_abcd_flags_bonds(
  financial_data,
  factset_entity_id__ar_company_id,
  factset_entity_id__security_mapped_sector,
  ar_company_id__sectors_with_assets__debt,
  factset_entity_id__credit_parent_id
) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "abcd_flags_bonds.rds"))

rm(ar_company_id__sectors_with_assets__debt)
invisible(gc())


rm(financial_data)
rm(factset_entity_id__ar_company_id)
rm(factset_entity_id__security_mapped_sector)
rm(factset_entity_id__credit_parent_id)
invisible(gc())

logger::log_info("ABCD flags prepared.")


# fund data output -------------------------------------------------------------

logger::log_info("Preparing fund data.")

readRDS(factset_fund_data_path) %>%
  pacta.data.preparation::prepare_fund_data(threshold = 0) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "fund_data.rds"))


logger::log_info("Saving file: \"total_fund_list.rds\".")

readRDS(file.path(config[["data_prep_outputs_path"]], "fund_data.rds")) %>%
  pacta.data.preparation::prepare_total_fund_list() %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "total_fund_list.rds"))


logger::log_info("Saving file: \"isin_to_fund_table.rds\".")

pacta.data.preparation::prepare_isin_to_fund_table(
  isin_to_fund_table = readRDS(factset_isin_to_fund_table_path),
  fund_data = readRDS(file.path(config[["data_prep_outputs_path"]], "fund_data.rds"))
) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "isin_to_fund_table.rds"))


logger::log_info("Fund data prepared.")


# emission data output ---------------------------------------------------------

iss_company_emissions <-
  readRDS(factset_iss_emissions_data_path) %>%
  pacta.data.preparation::prepare_iss_company_emissions()

logger::log_info("Formatting and saving file: \"iss_entity_emission_intensities.rds\".")

factset_financial_data <-
  readRDS(factset_financial_data_path) %>%
  select(factset_entity_id, adj_price, adj_shares_outstanding) %>%
  filter(!is.na(factset_entity_id) & !is.na(adj_shares_outstanding))

factset_entity_info <-
  readRDS(factset_entity_info_path) %>%
  select(factset_entity_id, iso_country, sector_code, factset_sector_desc) %>%
  left_join(factset_financial_data, by = "factset_entity_id")

rm(factset_financial_data)
invisible(gc())

iss_entity_emission_intensities <-
  readRDS(factset_entity_financing_data_path) %>%
  left_join(factset_entity_info, by = "factset_entity_id") %>%
  filter(countrycode::countrycode(iso_country, "iso2c", "iso4217c") == currency) %>%
  left_join(currencies, by = "currency") %>%
  mutate(
    ff_mkt_val = if_else(!is.na(ff_mkt_val), ff_mkt_val * exchange_rate, NA_real_),
    ff_debt = if_else(!is.na(ff_debt), ff_debt * exchange_rate, NA_real_),
    currency = "USD"
  ) %>%
  select(-exchange_rate) %>%
  summarise(
    ff_mkt_val = mean(ff_mkt_val, na.rm = TRUE),
    ff_debt = mean(ff_debt, na.rm = TRUE),
    .by = "factset_entity_id"
  ) %>%
  inner_join(iss_company_emissions, by = "factset_entity_id") %>%
  transmute(
    factset_entity_id = factset_entity_id,
    emission_intensity_per_mkt_val = if_else(
      ff_mkt_val == 0,
      NA_real_,
      icc_total_emissions / ff_mkt_val
    ),
    emission_intensity_per_debt = if_else(
      ff_debt == 0,
      NA_real_,
      icc_total_emissions / ff_debt
    ),
    ff_mkt_val,
    ff_debt,
    units = paste0(icc_total_emissions_units, " / ", "$ USD")
  )

saveRDS(
  select(iss_entity_emission_intensities, -c("ff_mkt_val", "ff_debt")),
  file.path(config[["data_prep_outputs_path"]], "iss_entity_emission_intensities.rds")
)


logger::log_info("Formatting and saving file: \"iss_average_sector_emission_intensities.rds\".")

iss_entity_emission_intensities %>%
  inner_join(factset_entity_info, by = "factset_entity_id") %>%
  summarise(
    emission_intensity_per_mkt_val = weighted.mean(
      emission_intensity_per_mkt_val,
      ff_mkt_val,
      na.rm = TRUE
    ),
    emission_intensity_per_debt = weighted.mean(
      emission_intensity_per_debt,
      ff_debt,
      na.rm = TRUE
    ),
    .by = c("sector_code", "factset_sector_desc", "units")
  ) %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "iss_average_sector_emission_intensities.rds"))


rm(iss_company_emissions)
rm(iss_entity_emission_intensities)
rm(factset_entity_info)
invisible(gc())

logger::log_info("Emissions data prepared.")


# combined ABCD and scenarios output -------------------------------------------

logger::log_info("Preparing combined ABCD scenario output.")

masterdata_ownership_datastore <-
  readRDS(file.path(config[["data_prep_outputs_path"]], "masterdata_ownership_datastore.rds")) %>%
  filter(year %in% relevant_years)

for (scenario_source in unique(scenarios_long$scenario_source)) {
  filename <- paste0("equity_abcd_scenario_", scenario_source, ".rds")
  scenarios_long_source <- filter(scenarios_long, .data$scenario_source == .env$scenario_source)
  logger::log_info("Formatting and saving file: \"{filename}\".")
  pacta.data.preparation::dataprep_abcd_scen_connection(
    abcd_data = masterdata_ownership_datastore,
    scenario_data = scenarios_long_source,
    reference_year = config[["market_share_target_reference_year"]],
    relevant_years = relevant_years,
    tech_exclude = config[["tech_exclude"]],
    scenario_geographies_list = config[["scenario_geographies_list"]],
    sector_list = config[["sector_list"]],
    other_sector_list = config[["other_sector_list"]],
    global_aggregate_scenario_sources_list = config[["global_aggregate_scenario_sources_list"]],
    global_aggregate_sector_list = config[["global_aggregate_sector_list"]],
    scenario_regions = scenario_regions,
    index_regions = index_regions
  ) %>%
    saveRDS(file.path(config[["data_prep_outputs_path"]], filename))
  invisible(gc())
}

rm(masterdata_ownership_datastore)
invisible(gc())

logger::log_info("Formatting and saving file: \"equity_abcd_scenario.rds\".")
list.files(
  config[["data_prep_outputs_path"]],
  pattern = "^equity_abcd_scenario_",
  full.names = TRUE
) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "equity_abcd_scenario.rds"))


masterdata_debt_datastore <-
  readRDS(file.path(config[["data_prep_outputs_path"]], "masterdata_debt_datastore.rds")) %>%
  filter(year %in% relevant_years)

for (scenario_source in unique(scenarios_long$scenario_source)) {
  filename <- paste0("bonds_abcd_scenario_", scenario_source, ".rds")
  scenarios_long_source <- filter(scenarios_long, .data$scenario_source == .env$scenario_source)
  logger::log_info("Formatting and saving file: \"{filename}\".")
  pacta.data.preparation::dataprep_abcd_scen_connection(
    abcd_data = masterdata_debt_datastore,
    scenario_data = scenarios_long_source,
    reference_year = config[["market_share_target_reference_year"]],
    relevant_years = relevant_years,
    tech_exclude = config[["tech_exclude"]],
    scenario_geographies_list = config[["scenario_geographies_list"]],
    sector_list = config[["sector_list"]],
    other_sector_list = config[["other_sector_list"]],
    global_aggregate_scenario_sources_list = config[["global_aggregate_scenario_sources_list"]],
    global_aggregate_sector_list = config[["global_aggregate_sector_list"]],
    scenario_regions = scenario_regions,
    index_regions = index_regions
  ) %>%
    saveRDS(file.path(config[["data_prep_outputs_path"]], filename))
  invisible(gc())
}

rm(masterdata_debt_datastore)
invisible(gc())

logger::log_info("Formatting and saving file: \"bonds_abcd_scenario.rds\".")
list.files(
  config[["data_prep_outputs_path"]],
  pattern = "^bonds_abcd_scenario_",
  full.names = TRUE
) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  saveRDS(file.path(config[["data_prep_outputs_path"]], "bonds_abcd_scenario.rds"))

logger::log_info("Combined ABCD scenario output prepared.")


# export SQLite versions of relevant files -------------------------------------

if (config[["export_sqlite_files"]]) {
  # entity_info
  logger::log_info("Formatting and saving file: \"entity_info.sqlite\".")

  entity_info <- readRDS(file.path(config[["data_prep_outputs_path"]], "entity_info.rds"))

  con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = file.path(config[["data_prep_outputs_path"]], "entity_info.sqlite")
    )
  RSQLite::sqliteSetBusyHandler(con, 3000L)

  dplyr::copy_to(
    dest = con,
    df = entity_info,
    name = "entity_info",
    overwrite = TRUE,
    temporary = FALSE,
    indexes = list("factset_entity_id")
  )

  DBI::dbDisconnect(con)
  rm(entity_info)
  invisible(gc())

  # equity_abcd_scenario
  logger::log_info("Formatting and saving file: \"equity_abcd_scenario.sqlite\".")

  equity_abcd_scenario <- readRDS(file.path(config[["data_prep_outputs_path"]], "equity_abcd_scenario.rds"))

  con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = file.path(config[["data_prep_outputs_path"]], "equity_abcd_scenario.sqlite")
    )
  RSQLite::sqliteSetBusyHandler(con, 3000L)

  dplyr::copy_to(
    dest = con,
    df = equity_abcd_scenario,
    name = "abcd_scenario",
    temporary = FALSE,
    overwrite = TRUE,
    indexes = list(
      "id",
      "equity_market",
      "scenario_source",
      "scenario_geography",
      "ald_sector"
    )
  )

  DBI::dbDisconnect(con)
  rm(equity_abcd_scenario)
  invisible(gc())

  # bonds_abcd_scenario
  logger::log_info("Formatting and saving file: \"bonds_abcd_scenario.sqlite\".")

  bonds_abcd_scenario <- readRDS(file.path(config[["data_prep_outputs_path"]], "bonds_abcd_scenario.rds"))

  con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = file.path(config[["data_prep_outputs_path"]], "bonds_abcd_scenario.sqlite")
    )
  RSQLite::sqliteSetBusyHandler(con, 3000L)

  dplyr::copy_to(
    dest = con,
    df = bonds_abcd_scenario,
    name = "abcd_scenario",
    temporary = FALSE,
    overwrite = TRUE,
    indexes = list(
      "id",
      "equity_market",
      "scenario_source",
      "scenario_geography",
      "ald_sector"
    )
  )

  DBI::dbDisconnect(con)
  rm(bonds_abcd_scenario)
  invisible(gc())
}


# manifests of input and output file -------------------------------------------

logger::log_info("Formatting and saving file: \"manifest.json\".")

ent_entity_affiliates_last_update <-
  readRDS(factset_entity_info_path) %>%
  filter(!is.na(ent_entity_affiliates_last_update)) %>%
  pull(ent_entity_affiliates_last_update) %>%
  unique()

# include PACTA packages NEWS.md test in the parameters to export
pacta_packages <- c("pacta.data.preparation", "pacta.scenario.preparation")
package_news <-
  vapply(
    X = pacta_packages,
    FUN = function(pkg) {
      pkg_news_file <- system.file("NEWS.md", package = pkg)
      list(paste0(readLines(pkg_news_file), collapse = "\n"))
    },
    FUN.VALUE = list(1),
    USE.NAMES = TRUE
  )

parameters <-
  list(
    config_name = config_name,
    raw_config = unclass(raw_config),
    config = unclass(config),
    input_filepaths = as.list(input_filepaths),
    timestamps = list(
      imf_quarter_timestamp = config[["imf_quarter_timestamp"]],
      factset_data_identifier = factset_timestamp,
      ent_entity_affiliates_last_update = ent_entity_affiliates_last_update,
      pacta_financial_timestamp = config[["pacta_financial_timestamp"]]
    ),
    scenarios = list(
      scenario_sources_list = config[["scenario_sources_list"]],
      scenario_geographies_list = config[["scenario_geographies_list"]],
      global_aggregate_scenario_sources_list = config[["global_aggregate_scenario_sources_list"]]
    ),
    sectors = list(
      sector_list = config[["sector_list"]],
      other_sector_list = config[["other_sector_list"]],
      global_aggregate_sector_list = config[["global_aggregate_sector_list"]]
    ),
    years = list(
      market_share_target_reference_year = config[["market_share_target_reference_year"]],
      time_horizon = config[["time_horizon"]],
      relevant_years = relevant_years
    ),
    technologies = list(
      zero_emission_factor_techs = config[["zero_emission_factor_techs"]],
      green_techs = config[["green_techs"]],
      tech_exclude = config[["tech_exclude"]]
    ),
    package_news = package_news
  )

logger::log_trace("Getting list of output files.")
output_files <- normalizePath(
  list.files(
    config[["data_prep_outputs_path"]],
    full.names = TRUE,
    recursive = TRUE
  )
)

manifest_path <- file.path(config[["data_prep_outputs_path"]], "manifest.json")
logger::log_trace("Writing manifest file: \"{manifest_path}\".")
pacta.data.preparation::write_manifest(
  path = manifest_path,
  parameters = parameters,
  input_files = input_filepaths,
  output_files = output_files
)
output_files <- c(output_files, manifest_path = manifest_path)

# copy in NEWs.md files from relevant PACTA packages ---------------------------

logger::log_info("Copying NEWS.md files from relevant PACTA packages.")

# `pacta_packages` defined above to add NEWS text to manifest
for (pkg_name in pacta_packages) {
  file.copy(
    system.file("NEWS.md", package = pkg_name),
    to = file.path(config[["data_prep_outputs_path"]], paste0(pkg_name, "-NEWS.md"))
  )
}

# Create archive files
if (config[["export_archives"]]) {
  logger::log_info("Exporting input and output archives.")

  logger::log_debug("Creating inputs zip file.")
  inputs_zip_file_path <- paste0(config[["data_prep_outputs_path"]], "_inputs.zip")
  logger::log_trace("Zip file path: \"{inputs_zip_file_path}\".")
  zip(
    zipfile = inputs_zip_file_path,
    files = normalizePath(unlist(parameters[["input_filepaths"]])),
    extras = c(
      "--junk-paths", # do not preserve paths
      "--no-dir-entries", # do not include directory entries
      "--quiet" # do not print progress to stdout
    )
  )
  logger::log_debug("Inputs archive created.")

  logger::log_debug("Creating outputs zip file.")
  outputs_zip_file_path <- paste0(config[["data_prep_outputs_path"]], ".zip")
  logger::log_trace("Zip file path: \"{outputs_zip_file_path}\".")
  zip(
    zipfile = outputs_zip_file_path,
    files = output_files,
    extras = c(
      "--junk-paths", # do not preserve paths
      "--no-dir-entries", # do not include directory entries
      "--quiet" # do not print progress to stdout
    )
  )
  logger::log_debug("Outputs archive created.")
}

# ------------------------------------------------------------------------------

logger::log_info("PACTA Data Preparation Complete.")
