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
  library(stringr)
  library(tidyr)

  # used for logging
  library(rlog)
  if (interactive()) Sys.setenv("LOG_LEVEL" = "ERROR")
})


# config -----------------------------------------------------------------------

readRenviron(".env")

config <-
  config::get(
    file = "config.yml",
    config = Sys.getenv("R_CONFIG_ACTIVE"),
    use_parent = FALSE
  )

data_prep_inputs_path <- config$data_prep_inputs_path
data_prep_outputs_path <- config$data_prep_outputs_path
masterdata_ownership_filename <- config$masterdata_ownership_filename
masterdata_debt_filename <- config$masterdata_debt_filename
ar_company_id__factset_entity_id_filename <- config$ar_company_id__factset_entity_id_filename
dbname <- config$dbname
host <- config$host
username <- Sys.getenv("R_DATABASE_USER")
password <- Sys.getenv("R_DATABASE_PASSWORD")
update_factset <- config$update_factset
update_currencies <- config$update_currencies
export_sqlite_files <- config$export_sqlite_files
imf_quarter_timestamp <- config$imf_quarter_timestamp
factset_data_timestamp <- config$factset_data_timestamp
pacta_financial_timestamp <- config$pacta_financial_timestamp
market_share_target_reference_year <- config$market_share_target_reference_year
time_horizon <- config$time_horizon
tdm_delta_years <- config$tdm_delta_years
scenario_sources_list <- config$scenario_sources_list
sector_list <- config$sector_list
other_sector_list <- config$other_sector_list
zero_emission_factor_techs <- config$zero_emission_factor_techs
green_techs <- config$green_techs
scenario_raw_data_to_include <- config$scenario_raw_data_to_include
tech_exclude <- config$tech_exclude
scenario_geographies_list <- config$scenario_geographies_list
global_aggregate_scenario_sources_list <- config$global_aggregate_scenario_sources_list
global_aggregate_sector_list <- config$global_aggregate_sector_list


# input filepaths --------------------------------------------------------------

masterdata_ownership_path <-
  file.path(data_prep_inputs_path, masterdata_ownership_filename)
masterdata_debt_path <-
  file.path(data_prep_inputs_path, masterdata_debt_filename)
ar_company_id__factset_entity_id_path <-
  file.path(data_prep_inputs_path, ar_company_id__factset_entity_id_filename)


# pre-flight filepaths ---------------------------------------------------------

scenarios_analysis_input_path <- file.path(data_prep_inputs_path, "Scenarios_AnalysisInput.csv")
scenario_regions_path <- file.path(data_prep_inputs_path, "scenario_regions.csv")
currencies_data_path <- file.path(data_prep_inputs_path, "currencies.rds")
factset_financial_data_path <- file.path(data_prep_inputs_path, "factset_financial_data.rds")
factset_entity_info_path <- file.path(data_prep_inputs_path, "factset_entity_info.rds")
factset_entity_financing_data_path <- file.path(data_prep_inputs_path, "factset_entity_financing_data.rds")
factset_fund_data_path <- file.path(data_prep_inputs_path, "factset_fund_data.rds")
factset_isin_to_fund_table_path <- file.path(data_prep_inputs_path, "factset_isin_to_fund_table.rds")
factset_iss_emissions_data_path <- file.path(data_prep_inputs_path, "factset_iss_emissions.rds")


# computed options -------------------------------------------------------------

relevant_years <- sort(
  unique(
    c(
      market_share_target_reference_year:(market_share_target_reference_year + time_horizon),
      market_share_target_reference_year + tdm_delta_years
    )
  )
)
log_info(
  paste(
    "Full time horizon set to:",
    paste0(relevant_years, collapse = ", ")
  )
)

scenario_raw_data_to_include <- lapply(scenario_raw_data_to_include, get, envir = asNamespace("pacta.scenario.preparation"))


# check that everything is ready to go -----------------------------------------

stopifnot(file.exists(masterdata_ownership_path))
stopifnot(file.exists(masterdata_debt_path))
stopifnot(file.exists(ar_company_id__factset_entity_id_path))

if (!update_currencies) {
  stopifnot(file.exists(currencies_data_path))
}

if (!update_factset) {
  stopifnot(file.exists(factset_financial_data_path))
  stopifnot(file.exists(factset_entity_info_path))
  stopifnot(file.exists(factset_entity_financing_data_path))
  stopifnot(file.exists(factset_fund_data_path))
  stopifnot(file.exists(factset_isin_to_fund_table_path))
  stopifnot(file.exists(factset_iss_emissions_data_path))
}


# pre-flight -------------------------------------------------------------------

log_info("Fetching pre-flight data... ")


log_info("Preparing scenario data... ")
scenario_raw_data <- bind_rows(scenario_raw_data_to_include)

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

scenario_raw_data %>%
  pacta.scenario.preparation::interpolate_yearly(!!!rlang::syms(interpolation_groups)) %>%
  filter(.data$year >= .env$market_share_target_reference_year) %>%
  pacta.scenario.preparation::add_market_share_columns(reference_year = market_share_target_reference_year) %>%
  pacta.scenario.preparation::format_p4i(green_techs) %>%
  write_csv(scenarios_analysis_input_path, na = "")

pacta.scenario.preparation::scenario_regions %>%
  write_csv(scenario_regions_path, na = "")


if (update_currencies) {
  log_info("Fetching currency data... ")
  pacta.data.scraping::get_currency_exchange_rates(
    quarter = imf_quarter_timestamp
  ) %>%
    saveRDS(currencies_data_path)
}


if (update_factset) {
  log_info("Fetching financial data... ")
  pacta.data.preparation::get_factset_financial_data(
    data_timestamp = factset_data_timestamp,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_financial_data_path)

  log_info("Fetching entity info data... ")
  pacta.data.preparation::get_factset_entity_info(
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_entity_info_path)

  log_info("Fetching entity financing data... ")
  pacta.data.preparation::get_factset_entity_financing_data(
    data_timestamp = factset_data_timestamp,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_entity_financing_data_path)

  log_info("Fetching fund data... ")
  pacta.data.preparation::get_factset_fund_data(
    data_timestamp = factset_data_timestamp,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_fund_data_path)

  log_info("Fetching fund ISINs... ")
  pacta.data.preparation::get_factset_isin_to_fund_table(
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_isin_to_fund_table_path)

  log_info("Fetching ISS emissions data... ")
  pacta.data.preparation::get_factset_iss_emissions_data(
    # IMPORTANT: `year` is 2019 on purpose as per a decision point from analysts.
    # See Issue #117 for more information.
    year = 2019,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_iss_emissions_data_path)
}

log_info("Pre-flight data prepared.")


# intermediary files -----------------------------------------------------------

log_info("Preparing scenario data... ")

scenario_regions <- readr::read_csv(scenario_regions_path, na = "", show_col_types = FALSE)

log_info("Scraping index regions... ")

index_regions <- pacta.data.scraping::get_index_regions()

factset_issue_code_bridge <-
  pacta.data.preparation::factset_issue_code_bridge %>%
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
  pacta.data.preparation::factset_industry_map_bridge %>%
  select(factset_industry_code, pacta_sector)

# scenarios_analysisinput_inputs
scenario_raw <- readr::read_csv(scenarios_analysis_input_path, show_col_types = FALSE)

# filter for relevant scenario data
scenarios_long <- scenario_raw %>%
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
    .data$scenario_source %in% .env$scenario_sources_list,
    .data$ald_sector %in% c(.env$sector_list, .env$other_sector_list),
    .data$scenario_geography %in% unique(.env$scenario_regions$scenario_geography),
    .data$year %in% unique(
      c(.env$relevant_years, .env$market_share_target_reference_year + 10)
    )
  )

log_info("Scenario data prepared.")


# currency data output ---------------------------------------------------------

log_info("Saving currencies.rds... ")

readRDS(currencies_data_path) %>%
  saveRDS(file.path(data_prep_outputs_path, "currencies.rds"))


# financial data output --------------------------------------------------------

log_info("Preparing financial data... ")

# read raw FactSet financial data, filter to unique rows, merge AR company_id,
# merge PACTA sectors from AR data
log_info("Formatting and saving financial_data.rds... ")

readRDS(factset_financial_data_path) %>%
  pacta.data.preparation::prepare_financial_data(factset_issue_code_bridge) %>%
  saveRDS(file.path(data_prep_outputs_path, "financial_data.rds"))

log_info("Formatting and saving entity_financing.rds... ")

readRDS(factset_entity_financing_data_path) %>%
  saveRDS(file.path(data_prep_outputs_path, "entity_financing.rds"))

log_info("Formatting and saving entity_info.rds... ")

factset_entity_id__ar_company_id <-
  readr::read_csv(ar_company_id__factset_entity_id_path, col_types = "c") %>%
  select(
    factset_entity_id = "factset_id",
    ar_company_id = "company_id"
  )

readRDS(factset_entity_info_path) %>%
  pacta.data.preparation::prepare_entity_info(factset_entity_id__ar_company_id) %>%
  saveRDS(file.path(data_prep_outputs_path, "entity_info.rds"))

log_info("Financial data prepared.")


# ABCD data output -------------------------------------------------------------

log_info("Preparing  ABCD... ")

entity_info <- readRDS(file.path(data_prep_outputs_path, "entity_info.rds"))

ar_company_id__country_of_domicile <-
  entity_info %>%
  select("ar_company_id", "country_of_domicile") %>%
  filter(!is.na(.data$ar_company_id)) %>%
  distinct()

ar_company_id__credit_parent_ar_company_id <-
  entity_info %>%
  select("ar_company_id", "credit_parent_ar_company_id") %>%
  filter(!is.na(.data$ar_company_id)) %>%
  distinct()

rm(entity_info)


log_info("Formatting and saving masterdata_ownership_datastore.rds... ")

readr::read_csv(masterdata_ownership_path, na = "", show_col_types = FALSE) %>%
  pacta.data.preparation::prepare_masterdata(
    ar_company_id__country_of_domicile,
    pacta_financial_timestamp,
    zero_emission_factor_techs
  ) %>%
  saveRDS(file.path(data_prep_outputs_path, "masterdata_ownership_datastore.rds"))


log_info("Formatting and saving masterdata_debt_datastore.rds... ")

masterdata_debt <- readr::read_csv(masterdata_debt_path, na = "", show_col_types = FALSE)

company_id__creditor_company_id <-
  masterdata_debt %>%
  select("company_id", "creditor_company_id") %>%
  distinct() %>%
  mutate(across(.cols = dplyr::everything(), .fns = as.character))

masterdata_debt %>%
  pacta.data.preparation::prepare_masterdata(
    ar_company_id__country_of_domicile,
    pacta_financial_timestamp,
    zero_emission_factor_techs
  ) %>%
  left_join(company_id__creditor_company_id, by = c(id = "company_id")) %>%
  left_join(ar_company_id__credit_parent_ar_company_id, by = c(id = "ar_company_id")) %>%
  mutate(id = if_else(!is.na(.data$creditor_company_id), .data$creditor_company_id, .data$id)) %>%
  mutate(id = if_else(!is.na(.data$credit_parent_ar_company_id), .data$credit_parent_ar_company_id, .data$id)) %>%
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
  saveRDS(file.path(data_prep_outputs_path, "masterdata_debt_datastore.rds"))

rm(masterdata_debt)
rm(company_id__creditor_company_id)

rm(ar_company_id__country_of_domicile)
rm(ar_company_id__credit_parent_ar_company_id)

log_info("ABCD prepared.")


# abcd_flags -------------------------------------------------------------------

log_info("Preparing  ABCD flags... ")
financial_data <- readRDS(file.path(data_prep_outputs_path, "financial_data.rds"))

entity_info <- readRDS(file.path(data_prep_outputs_path, "entity_info.rds"))

factset_entity_id__ar_company_id <-
  entity_info %>%
  select(factset_entity_id, ar_company_id) %>%
  filter(!is.na(ar_company_id))

factset_entity_id__security_mapped_sector <-
  entity_info %>%
  select(factset_entity_id, security_mapped_sector)


log_info("Formatting and saving abcd_flags_equity.rds... ")

ar_company_id__sectors_with_assets__ownership <-
  readRDS(file.path(data_prep_outputs_path, "masterdata_ownership_datastore.rds")) %>%
  filter(year %in% relevant_years) %>%
  select(ar_company_id = id, ald_sector) %>%
  distinct() %>%
  group_by(ar_company_id) %>%
  summarise(sectors_with_assets = paste(unique(ald_sector), collapse = " + "))

financial_data %>%
  left_join(factset_entity_id__ar_company_id, by = "factset_entity_id") %>%
  left_join(factset_entity_id__security_mapped_sector, by = "factset_entity_id") %>%
  left_join(ar_company_id__sectors_with_assets__ownership, by = "ar_company_id") %>%
  mutate(has_asset_level_data = if_else(is.na(sectors_with_assets) | sectors_with_assets == "", FALSE, TRUE)) %>%
  mutate(has_ald_in_fin_sector = if_else(stringr::str_detect(sectors_with_assets, security_mapped_sector), TRUE, FALSE)) %>%
  select(
    isin,
    has_asset_level_data,
    has_ald_in_fin_sector,
    sectors_with_assets
  ) %>%
  saveRDS(file.path(data_prep_outputs_path, "abcd_flags_equity.rds"))


log_info("Formatting and saving abcd_flags_bonds.rds...  ")

ar_company_id__sectors_with_assets__debt <-
  readRDS(file.path(data_prep_outputs_path, "masterdata_debt_datastore.rds")) %>%
  filter(year %in% relevant_years) %>%
  select(ar_company_id = id, ald_sector) %>%
  distinct() %>%
  group_by(ar_company_id) %>%
  summarise(sectors_with_assets = paste(unique(ald_sector), collapse = " + "))

financial_data %>%
  left_join(factset_entity_id__ar_company_id, by = "factset_entity_id") %>%
  left_join(factset_entity_id__security_mapped_sector, by = "factset_entity_id") %>%
  left_join(ar_company_id__sectors_with_assets__debt, by = "ar_company_id") %>%
  mutate(has_asset_level_data = if_else(is.na(sectors_with_assets) | sectors_with_assets == "", FALSE, TRUE)) %>%
  mutate(has_ald_in_fin_sector = if_else(stringr::str_detect(sectors_with_assets, security_mapped_sector), TRUE, FALSE)) %>%
  left_join(
    select(entity_info, "factset_entity_id", "credit_parent_id"),
    by = "factset_entity_id"
  ) %>%
  mutate(
    # If FactSet has no credit_parent, we define the company as it's own parent
    credit_parent_id = if_else(is.na(credit_parent_id), factset_entity_id, credit_parent_id)
  ) %>%
  group_by(credit_parent_id) %>%
  summarise(
    has_asset_level_data = sum(has_asset_level_data, na.rm = TRUE) > 0,
    has_ald_in_fin_sector = sum(has_ald_in_fin_sector, na.rm = TRUE) > 0,
    sectors_with_assets = paste(sort(unique(na.omit(unlist(str_split(sectors_with_assets, pattern = " [+] "))))), collapse = " + ")
  ) %>%
  ungroup() %>%
  saveRDS(file.path(data_prep_outputs_path, "abcd_flags_bonds.rds"))


rm(financial_data)
rm(entity_info)
rm(factset_entity_id__ar_company_id)
rm(factset_entity_id__security_mapped_sector)
log_info("ABCD flags prepared.")


# fund data output -------------------------------------------------------------

log_info("Preparing fund data... ")

fund_data <- readRDS(factset_fund_data_path)

# remove funds above the threshold
fund_data <-
  fund_data %>%
  group_by(factset_fund_id, fund_reported_mv) %>%
  filter((fund_reported_mv[[1]] - sum(holding_reported_mv)) / fund_reported_mv[[1]] > -1e-5) %>%
  ungroup()

# build MISSINGWEIGHT for under and over
fund_missing_mv <-
  fund_data %>%
  group_by(factset_fund_id, fund_reported_mv) %>%
  summarise(
    holding_isin = "MISSINGWEIGHT",
    holding_reported_mv = fund_reported_mv[[1]] - sum(holding_reported_mv),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  filter(holding_reported_mv != 0)

fund_data %>%
  bind_rows(fund_missing_mv) %>%
  saveRDS(file.path(data_prep_outputs_path, "fund_data.rds"))


log_info("Saving total_fund_list.rds... ")
fund_data %>%
  select(factset_fund_id) %>%
  distinct() %>%
  saveRDS(file.path(data_prep_outputs_path, "total_fund_list.rds"))


log_info("Saving isin_to_fund_table.rds... ")

isin_to_fund_table <- readRDS(factset_isin_to_fund_table_path)

# filter out fsyms that have more than 1 row and no fund data
isin_to_fund_table <-
  isin_to_fund_table %>%
  mutate(has_fund_data = factset_fund_id %in% fund_data$factset_fund_id) %>%
  group_by(fsym_id) %>%
  mutate(n = n()) %>%
  filter(n == 1 | (n > 1 & has_fund_data)) %>%
  ungroup() %>%
  select(-n, -has_fund_data)

# filter out fsyms that have more than 1 row and have fund data for both rows
isin_to_fund_table <-
  isin_to_fund_table %>%
  mutate(has_fund_data = factset_fund_id %in% fund_data$factset_fund_id) %>%
  group_by(fsym_id) %>%
  mutate(n = n()) %>%
  filter(!(all(has_fund_data) & n > 1)) %>%
  ungroup() %>%
  select(-n, -has_fund_data)

isin_to_fund_table %>%
  saveRDS(file.path(data_prep_outputs_path, "isin_to_fund_table.rds"))


rm(fund_data)
rm(isin_to_fund_table)

log_info("Fund data prepared.")


# emission data output ---------------------------------------------------------

currencies <- readRDS(file.path(data_prep_outputs_path, "currencies.rds"))

iss_company_emissions <-
  readRDS(factset_iss_emissions_data_path) %>%
  group_by(factset_entity_id) %>%
  summarise(
    icc_total_emissions = sum(icc_total_emissions + icc_scope_3_emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(icc_total_emissions_units = "tCO2e") # units are defined in the ISS/FactSet documentation (see #144)

log_info("Formatting and saving iss_entity_emission_intensities.rds...  ")

iss_entity_emission_intensities <-
  readRDS(factset_entity_financing_data_path) %>%
  left_join(currencies, by = "currency") %>%
  mutate(
    ff_mkt_val = ff_mkt_val * exchange_rate,
    ff_debt = ff_debt * exchange_rate,
    currency = "USD"
  ) %>%
  select(-exchange_rate) %>%
  group_by(factset_entity_id, currency) %>%
  summarise(
    ff_mkt_val = sum(ff_mkt_val, na.rm = TRUE),
    ff_debt = sum(ff_debt, na.rm = TRUE),
    .groups = "drop"
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
  file.path(data_prep_outputs_path, "iss_entity_emission_intensities.rds")
)


log_info("Formatting and saving iss_average_sector_emission_intensities.rds...  ")

factset_entity_info <- readRDS(factset_entity_info_path)

iss_entity_emission_intensities %>%
  inner_join(factset_entity_info, by = "factset_entity_id") %>%
  group_by(sector_code, factset_sector_desc, units) %>%
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
    .groups = "drop"
  ) %>%
  ungroup() %>%
  saveRDS(file.path(data_prep_outputs_path, "iss_average_sector_emission_intensities.rds"))


rm(currencies)
rm(iss_company_emissions)
rm(iss_entity_emission_intensities)
rm(factset_entity_info)

log_info("Emissions data prepared.")


# combined ABCD and scenarios output -------------------------------------------

log_info("Preparing combined ABCD scenario output... ")

masterdata_ownership_datastore <-
  readRDS(file.path(data_prep_outputs_path, "masterdata_ownership_datastore.rds")) %>%
  filter(year %in% relevant_years)

for (scenario_source in unique(scenarios_long$scenario_source)) {
  filename <- paste0("equity_abcd_scenario_", scenario_source, ".rds")
  scenarios_long_source <- filter(scenarios_long, .data$scenario_source == .env$scenario_source)
  log_info(paste0("Formatting and saving ", filename, "... "))
  pacta.data.preparation::dataprep_abcd_scen_connection(
    abcd_data = masterdata_ownership_datastore,
    scenario_data = scenarios_long_source,
    reference_year = market_share_target_reference_year,
    relevant_years = relevant_years,
    tech_exclude = tech_exclude,
    scenario_geographies_list = scenario_geographies_list,
    sector_list = sector_list,
    other_sector_list = other_sector_list,
    global_aggregate_scenario_sources_list = global_aggregate_scenario_sources_list,
    global_aggregate_sector_list = global_aggregate_sector_list,
    scenario_regions = scenario_regions,
    index_regions = index_regions
  ) %>%
    saveRDS(file.path(data_prep_outputs_path, filename))
}

log_info("Formatting and saving equity_abcd_scenario.rds... ")
list.files(
  data_prep_outputs_path,
  pattern = "^equity_abcd_scenario_",
  full.names = TRUE
) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  saveRDS(file.path(data_prep_outputs_path, "equity_abcd_scenario.rds"))


masterdata_debt_datastore <-
  readRDS(file.path(data_prep_outputs_path, "masterdata_debt_datastore.rds")) %>%
  filter(year %in% relevant_years)

for (scenario_source in unique(scenarios_long$scenario_source)) {
  filename <- paste0("bonds_abcd_scenario_", scenario_source, ".rds")
  scenarios_long_source <- filter(scenarios_long, .data$scenario_source == .env$scenario_source)
  log_info(paste0("Formatting and saving ", filename, "... "))
  pacta.data.preparation::dataprep_abcd_scen_connection(
    abcd_data = masterdata_debt_datastore,
    scenario_data = scenarios_long_source,
    reference_year = market_share_target_reference_year,
    relevant_years = relevant_years,
    tech_exclude = tech_exclude,
    scenario_geographies_list = scenario_geographies_list,
    sector_list = sector_list,
    other_sector_list = other_sector_list,
    global_aggregate_scenario_sources_list = global_aggregate_scenario_sources_list,
    global_aggregate_sector_list = global_aggregate_sector_list,
    scenario_regions = scenario_regions,
    index_regions = index_regions
  ) %>%
    saveRDS(file.path(data_prep_outputs_path, filename))
}

log_info("Formatting and saving bonds_abcd_scenario.rds... ")
list.files(
  data_prep_outputs_path,
  pattern = "^bonds_abcd_scenario_",
  full.names = TRUE
) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  saveRDS(file.path(data_prep_outputs_path, "bonds_abcd_scenario.rds"))

log_info("Combined ABCD scenario output prepared.")


# export SQLite versions of relevant files -------------------------------------

if (export_sqlite_files) {
  # entity_info
  log_info("Formatting and saving entity_info.sqlite... ")

  entity_info <- readRDS(file.path(data_prep_outputs_path, "entity_info.rds"))

  con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = file.path(data_prep_outputs_path, "entity_info.sqlite")
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

  # equity_abcd_scenario
  log_info("Formatting and saving equity_abcd_scenario.sqlite... ")

  equity_abcd_scenario <- readRDS(file.path(data_prep_outputs_path, "equity_abcd_scenario.rds"))

  con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = file.path(data_prep_outputs_path, "equity_abcd_scenario.sqlite")
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

  # bonds_abcd_scenario
  log_info("Formatting and saving bonds_abcd_scenario.sqlite... ")

  bonds_abcd_scenario <- readRDS(file.path(data_prep_outputs_path, "bonds_abcd_scenario.rds"))

  con <-
    DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = file.path(data_prep_outputs_path, "bonds_abcd_scenario.sqlite")
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
}


# manifests of input and output file -------------------------------------------

log_info("Formatting and saving manifest.json... ")

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
    input_filepaths = list(
      masterdata_ownership_path = masterdata_ownership_path,
      masterdata_debt_path = masterdata_debt_path,
      ar_company_id__factset_entity_id_path = ar_company_id__factset_entity_id_path
    ),
    preflight_filepaths = list(
      scenarios_analysis_input_path = scenarios_analysis_input_path,
      scenario_regions_path = scenario_regions_path,
      currencies_data_path = currencies_data_path,
      factset_financial_data_path = factset_financial_data_path,
      factset_entity_info_path = factset_entity_info_path,
      factset_fund_data_path = factset_fund_data_path,
      factset_isin_to_fund_table_path = factset_isin_to_fund_table_path
    ),
    factset_database = list(
      dbname = dbname,
      host = host,
      username = username,
      ent_entity_affiliates_last_update = ent_entity_affiliates_last_update
    ),
    timestamps = list(
      imf_quarter_timestamp = imf_quarter_timestamp,
      factset_data_timestamp = factset_data_timestamp,
      pacta_financial_timestamp = pacta_financial_timestamp
    ),
    scenarios = list(
      scenario_sources_list = scenario_sources_list,
      scenario_geographies_list = scenario_geographies_list,
      global_aggregate_scenario_sources_list = global_aggregate_scenario_sources_list
    ),
    sectors = list(
      sector_list = sector_list,
      other_sector_list = other_sector_list,
      global_aggregate_sector_list = global_aggregate_sector_list
    ),
    years = list(
      market_share_target_reference_year = market_share_target_reference_year,
      time_horizon = time_horizon,
      relevant_years = relevant_years
    ),
    technologies = list(
      zero_emission_factor_techs = zero_emission_factor_techs,
      green_techs = green_techs,
      tech_exclude = tech_exclude
    ),
    update_factset = update_factset,
    package_news = package_news
  )

pacta.data.preparation::write_manifest(
  path = file.path(data_prep_outputs_path, "manifest.json"),
  parameters = parameters,
  data_prep_inputs_path = data_prep_inputs_path,
  data_prep_outputs_path = data_prep_outputs_path
)


# copy in NEWs.md files from relevant PACTA packages ---------------------------

log_info("Copying NEW.md files from relevant PACTA packages... ")

# `pacta_packages` defined above to add NEWS text to manifest
for (pkg_name in pacta_packages) {
  file.copy(
    system.file("NEWS.md", package = pkg_name),
    to = file.path(data_prep_outputs_path, paste0(pkg_name, "-NEWS.md"))
  )
}


# ------------------------------------------------------------------------------

log_info("PACTA Data Preparation Complete.")
