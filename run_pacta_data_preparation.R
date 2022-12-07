# necessary packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(pacta.data.preparation)
  library(pacta.scenario.preparation)

  library(dplyr)
  library(readr)
  library(rlang)
  library(stringr)
  library(tidyr)

  # used for logging
  library(rlog)
  if (interactive()) Sys.setenv("LOG_LEVEL" = "ERROR")
})


# config -----------------------------------------------------------------------

config <- config::get(file = "config.yml", use_parent = FALSE)

data_prep_inputs_path <- config$data_prep_inputs_path
data_prep_outputs_path <- config$data_prep_outputs_path
masterdata_ownership_filename <- config$masterdata_ownership_filename
masterdata_debt_filename <- config$masterdata_debt_filename
ar_company_id__factset_entity_id_filename <- config$ar_company_id__factset_entity_id_filename
dbname <- config$dbname
host <- config$host
username <- config$username
password <- config$password
update_factset <- config$update_factset
imf_quarter_timestamp <- config$imf_quarter_timestamp
factset_data_timestamp <- config$factset_data_timestamp
pacta_financial_timestamp <- config$pacta_financial_timestamp
indices_timestamp <- config$indices_timestamp
market_share_target_reference_year <- config$market_share_target_reference_year
time_horizon <- config$time_horizon
additional_year <- config$additional_year
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
ishares_indices_bonds_data_path <- file.path(data_prep_inputs_path, "ishares_indices_bonds_data.rds")
ishares_indices_equity_data_path <- file.path(data_prep_inputs_path, "ishares_indices_equity_data.rds")


# computed options -------------------------------------------------------------

relevant_years <- sort(
  unique(
    c(
      market_share_target_reference_year:(market_share_target_reference_year + time_horizon),
      additional_year
    )
  )
)
log_info(
  paste(
    "Full time horizon set to:",
    paste0(relevant_years, collapse = ", ")
  )
)

scenario_raw_data_to_include <- lapply(scenario_raw_data_to_include, get)

# names and and urls of iShares indices
equity_indices_urls <-
  c(
    "iShares Core S&P 500 UCITS ETF USD (Dist) <USD (Distributing)>" =
      "https://www.ishares.com/uk/individual/en/products/251900/ishares-sp-500-ucits-etf-inc-fund/",

    "iShares MSCI World UCITS ETF <USD (Distributing)>" =
      "https://www.ishares.com/uk/individual/en/products/251881/ishares-msci-world-ucits-etf-inc-fund/",

    "iShares MSCI EM UCITS ETF USD (Acc)" =
      "https://www.ishares.com/uk/individual/en/products/251858/ishares-msci-emerging-markets-ucits-etf-acc-fund/",

    "iShares MSCI ACWI UCITS ETF <USD (Accumulating)>" =
      "https://www.ishares.com/uk/individual/en/products/251850/ishares-msci-acwi-ucits-etf/"
  )

bonds_indices_urls <-
  c(
    "iShares Global Corp Bond UCITS ETF <USD (Distributing)>" =
      "https://www.ishares.com/uk/individual/en/products/251813/ishares-global-corporate-bond-ucits-etf/"
  )


# check that everything is ready to go -----------------------------------------



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
  interpolate_yearly(!!!syms(interpolation_groups)) %>%
  filter(.data$year >= .env$market_share_target_reference_year) %>%
  add_market_share_columns(reference_year = market_share_target_reference_year) %>%
  format_p4i(green_techs) %>%
  write_csv(scenarios_analysis_input_path, na = "")

pacta.scenario.preparation::scenario_regions %>%
  write_csv(scenario_regions_path, na = "")

log_info("Fetching currency data... ")
pacta.data.preparation:::get_currency_exchange_rates(
  quarter = imf_quarter_timestamp
) %>%
  saveRDS(currencies_data_path)

if (config$update_factset) {
  log_info("Fetching financial data... ")
  pacta.data.preparation:::get_factset_financial_data(
    data_timestamp = factset_data_timestamp,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_financial_data_path)

  log_info("Fetching entity info data... ")
  pacta.data.preparation:::get_factset_entity_info(
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_entity_info_path)

  log_info("Fetching entity financing data... ")
  pacta.data.preparation:::get_factset_entity_financing_data(
    data_timestamp = factset_data_timestamp,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_entity_financing_data_path)

  log_info("Fetching fund data... ")
  pacta.data.preparation:::get_factset_fund_data(
    data_timestamp = factset_data_timestamp,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_fund_data_path)

  log_info("Fetching fund ISINs... ")
  pacta.data.preparation:::get_factset_isin_to_fund_table(
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_isin_to_fund_table_path)

  pacta.data.preparation:::get_factset_iss_emissions_data(
    #IMPORTANT: `year` is 2019 on purpose as per a decision point from analysts.
    # See Issue #117 for more information.
    year = 2019,
    dbname = dbname,
    host = host,
    username = username,
    password = password
  ) %>%
    saveRDS(factset_iss_emissions_data_path)
}

log_info("Fetching bonds indices... ")
dplyr::bind_rows(
  lapply(
    seq_along(bonds_indices_urls), function(index) {
      pacta.data.preparation:::get_ishares_index_data(
        bonds_indices_urls[[index]],
        names(bonds_indices_urls)[[index]],
        indices_timestamp
      )
    }
  )
) %>%
  saveRDS(ishares_indices_bonds_data_path)

log_info("Fetching equity indices... ")
dplyr::bind_rows(
  lapply(
    seq_along(equity_indices_urls), function(index) {
      pacta.data.preparation:::get_ishares_index_data(
        equity_indices_urls[[index]],
        names(equity_indices_urls)[[index]],
        indices_timestamp
      )
    }
  )
) %>%
  saveRDS(ishares_indices_equity_data_path)

log_info("Pre-flight data prepared.")


# intermediary files -----------------------------------------------------------

log_info("Preparing scenario data... ")

scenario_regions <- read_csv(scenario_regions_path, show_col_types = FALSE)

index_regions <- pacta.data.preparation::index_regions

factset_issue_code_bridge <- pacta.data.preparation::factset_issue_code_bridge %>%
  select("issue_type_code", "asset_type")

factset_industry_map_bridge <- pacta.data.preparation::factset_industry_map_bridge %>%
  select("factset_industry_code", "pacta_sector")

factset_manual_pacta_sector_override <- pacta.data.preparation::factset_manual_pacta_sector_override %>%
  select("factset_entity_id", "pacta_sector_override")

# scenarios_analysisinput_inputs
scenario_raw <- readr::read_csv(scenarios_analysis_input_path, show_col_types = FALSE)

# filter for relevant scenario data
scenarios_long <- scenario_raw %>%
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
  prepare_financial_data() %>%
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
  prepare_entity_info(factset_entity_id__ar_company_id) %>%
  saveRDS(file.path(data_prep_outputs_path, "entity_info.rds"))

log_info("Financial data prepared.")


# indices output ---------------------------------------------------------------

log_info("Processing bonds indices data... ")
readRDS(ishares_indices_bonds_data_path) %>%
  pacta.data.preparation:::process_ishares_index_data() %>%
  saveRDS(file.path(data_prep_outputs_path, "ishares_indices_bonds.rds"))

log_info("Processing equity indices data... ")
readRDS(ishares_indices_equity_data_path) %>%
  pacta.data.preparation:::process_ishares_index_data() %>%
  saveRDS(file.path(data_prep_outputs_path, "ishares_indices_equity.rds"))

log_info("Indices data prepared.")


# ABCD data output -------------------------------------------------------------

log_info("Preparing  ABCD... ")
ar_company_id__country_of_domicile <-
  readRDS(file.path(data_prep_outputs_path, "entity_info.rds")) %>%
  select("ar_company_id", "country_of_domicile") %>%
  filter(!is.na(.data$ar_company_id)) %>%
  distinct()

log_info("Formatting and saving masterdata_ownership_datastore.rds... ")

readr::read_csv(masterdata_ownership_path, show_col_types = FALSE) %>%
  prepare_masterdata(
    ar_company_id__country_of_domicile,
    pacta_financial_timestamp,
    zero_emission_factor_techs
  ) %>%
  saveRDS(file.path(data_prep_outputs_path, "masterdata_ownership_datastore.rds"))

log_info("Formatting and saving masterdata_debt_datastore.rds... ")

readr::read_csv(masterdata_debt_path, show_col_types = FALSE) %>%
  prepare_masterdata(
    ar_company_id__country_of_domicile,
    pacta_financial_timestamp,
    zero_emission_factor_techs
  ) %>%
  saveRDS(file.path(data_prep_outputs_path, "masterdata_debt_datastore.rds"))

log_info("ABCD prepared.")


# abcd_flags -------------------------------------------------------------------

log_info("Preparing  ABCD flags... ")
financial_data <- readRDS(file.path(data_prep_outputs_path, "financial_data.rds"))

entity_info <- readRDS(file.path(data_prep_outputs_path, "entity_info.rds"))

factset_entity_id__ar_company_id <-
  entity_info %>%
  select("factset_entity_id", "ar_company_id") %>%
  filter(!is.na(.data$ar_company_id))

factset_entity_id__security_mapped_sector <-
  entity_info %>%
  select("factset_entity_id", "security_mapped_sector")

log_info("Formatting and saving abcd_flags_equity.rds... ")

ar_company_id__sectors_with_assets__ownership <-
  readRDS(file.path(data_prep_outputs_path, "masterdata_ownership_datastore.rds")) %>%
  filter(.data$year %in% .env$relevant_years) %>%
  select(ar_company_id = "id", "ald_sector") %>%
  distinct() %>%
  group_by(.data$ar_company_id) %>%
  summarise(sectors_with_assets = paste(unique(.data$ald_sector), collapse = " + "))

financial_data %>%
  left_join(factset_entity_id__ar_company_id, by = "factset_entity_id") %>%
  left_join(factset_entity_id__security_mapped_sector, by = "factset_entity_id") %>%
  left_join(ar_company_id__sectors_with_assets__ownership, by = "ar_company_id") %>%
  mutate(has_asset_level_data = if_else(is.na(.data$sectors_with_assets) | .data$sectors_with_assets == "", FALSE, TRUE)) %>%
  mutate(has_ald_in_fin_sector = if_else(stringr::str_detect(.data$sectors_with_assets, .data$security_mapped_sector), TRUE, FALSE)) %>%
  select(
    "isin",
    "has_asset_level_data",
    "has_ald_in_fin_sector",
    "sectors_with_assets"
  ) %>%
  saveRDS(file.path(data_prep_outputs_path, "abcd_flags_equity.rds"))

log_info("Formatting and saving abcd_flags_bonds.rds...  ")

ar_company_id__sectors_with_assets__debt <-
  readRDS(file.path(data_prep_outputs_path, "masterdata_debt_datastore.rds")) %>%
  filter(.data$year %in% .env$relevant_years) %>%
  select(ar_company_id = "id", "ald_sector") %>%
  distinct() %>%
  group_by(.data$ar_company_id) %>%
  summarise(sectors_with_assets = paste(unique(.data$ald_sector), collapse = " + "))

financial_data %>%
  left_join(factset_entity_id__ar_company_id, by = "factset_entity_id") %>%
  left_join(factset_entity_id__security_mapped_sector, by = "factset_entity_id") %>%
  left_join(ar_company_id__sectors_with_assets__debt, by = "ar_company_id") %>%
  mutate(has_asset_level_data = if_else(is.na(.data$sectors_with_assets) | .data$sectors_with_assets == "", FALSE, TRUE)) %>%
  mutate(has_ald_in_fin_sector = if_else(stringr::str_detect(.data$sectors_with_assets, .data$security_mapped_sector), TRUE, FALSE)) %>%
  left_join(
    select(entity_info, "factset_entity_id", "credit_parent_id"),
    by = "factset_entity_id"
  ) %>%
  mutate(
    # If FactSet has no credit_parent, we define the company as it's own parent
    credit_parent_id = if_else(is.na(credit_parent_id), factset_entity_id, credit_parent_id)
    ) %>%
  group_by(.data$credit_parent_id) %>%
  summarise(
    has_asset_level_data = sum(.data$has_asset_level_data, na.rm = TRUE) > 0,
    has_ald_in_fin_sector = sum(.data$has_ald_in_fin_sector, na.rm = TRUE) > 0,
    sectors_with_assets = paste(sort(unique(na.omit(unlist(str_split(.data$sectors_with_assets, pattern = " [+] "))))), collapse = " + ")
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
  group_by(.data$factset_fund_id, .data$fund_reported_mv) %>%
  filter((.data$fund_reported_mv[[1]] - sum(.data$holding_reported_mv)) / .data$fund_reported_mv[[1]] > -1e-5) %>%
  ungroup()

# build MISSINGWEIGHT for under and over
fund_missing_mv <-
  fund_data %>%
  group_by(.data$factset_fund_id, .data$fund_reported_mv) %>%
  summarise(
    holding_isin = "MISSINGWEIGHT",
    holding_reported_mv = .data$fund_reported_mv[[1]] - sum(.data$holding_reported_mv),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  filter(.data$holding_reported_mv != 0)

fund_data %>%
  bind_rows(fund_missing_mv) %>%
  saveRDS(file.path(data_prep_outputs_path, "fund_data.rds"))

log_info("Saving total_fund_list.rds... ")
fund_data %>%
  select("factset_fund_id") %>%
  distinct() %>%
  saveRDS(file.path(data_prep_outputs_path, "total_fund_list.rds"))

log_info("Saving isin_to_fund_table.rds... ")

isin_to_fund_table <- readRDS(factset_isin_to_fund_table_path)

# filter out fsyms that have more than 1 row and no fund data
isin_to_fund_table <-
  isin_to_fund_table %>%
  mutate(has_fund_data = .data$factset_fund_id %in% fund_data$factset_fund_id) %>%
  group_by(.data$fsym_id) %>%
  mutate(n = n()) %>%
  filter(n == 1 | (n > 1 & .data$has_fund_data)) %>%
  ungroup() %>%
  select(-"n", -"has_fund_data")

# filter out fsyms that have more than 1 row and have fund data for both rows
isin_to_fund_table <-
  isin_to_fund_table %>%
  mutate(has_fund_data = .data$factset_fund_id %in% fund_data$factset_fund_id) %>%
  group_by(.data$fsym_id) %>%
  mutate(n = n()) %>%
  filter(!(all(.data$has_fund_data) & n > 1)) %>%
  ungroup() %>%
  select(-"n", -"has_fund_data")

isin_to_fund_table %>%
  saveRDS(file.path(data_prep_outputs_path, "isin_to_fund_table.rds"))

rm(fund_data)
rm(isin_to_fund_table)

log_info("Fund data prepared.")


# emission data output ---------------------------------------------------------

currencies <- readRDS(file.path(data_prep_outputs_path, "currencies.rds"))

iss_company_emissions <-
  readRDS(factset_iss_emissions_data_path) %>%
  group_by(.data$factset_entity_id) %>%
  summarise(
    icc_total_emissions = sum(.data$icc_total_emissions + .data$icc_scope_3_emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(icc_total_emissions_units = "tCO2e")  # units are defined in the ISS/FactSet documentation (see #144)

log_info("Formatting and saving iss_entity_emission_intensities.rds...  ")

iss_entity_emission_intensities <-
  readRDS(factset_entity_financing_data_path) %>%
  left_join(currencies, by = "currency") %>%
  mutate(
    ff_mkt_val = .data$ff_mkt_val * .data$exchange_rate,
    ff_debt = .data$ff_debt * .data$exchange_rate,
    currency = "USD"
  ) %>%
  select(-"exchange_rate") %>%
  group_by(.data$factset_entity_id, .data$currency) %>%
  summarise(
    ff_mkt_val = sum(.data$ff_mkt_val, na.rm = TRUE),
    ff_debt = sum(.data$ff_debt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(iss_company_emissions, by = "factset_entity_id") %>%
  transmute(
    factset_entity_id = .data$factset_entity_id,
    emission_intensity_per_mkt_val = if_else(
      .data$ff_mkt_val == 0,
      NA_real_,
      .data$icc_total_emissions / .data$ff_mkt_val
    ),
    emission_intensity_per_debt = if_else(
      .data$ff_debt == 0,
      NA_real_,
      .data$icc_total_emissions / .data$ff_debt
    ),
    ff_mkt_val = .data$ff_mkt_val,
    ff_debt = .data$ff_debt,
    units = paste0(.data$icc_total_emissions_units, " / ", "$ USD")
  )

saveRDS(
  select(iss_entity_emission_intensities, -c("ff_mkt_val", "ff_debt")),
  file.path(data_prep_outputs_path, "iss_entity_emission_intensities.rds")
  )

log_info("Formatting and saving iss_average_sector_emission_intensities.rds...  ")

factset_entity_info <- readRDS(factset_entity_info_path)

iss_entity_emission_intensities %>%
  inner_join(factset_entity_info, by = "factset_entity_id") %>%
  group_by(.data$sector_code, .data$factset_sector_desc, .data$units) %>%
  summarise(
    emission_intensity_per_mkt_val = weighted.mean(
      .data$emission_intensity_per_mkt_val,
      .data$ff_mkt_val,
      na.rm = TRUE
    ),
    emission_intensity_per_debt = weighted.mean(
      .data$emission_intensity_per_debt,
      .data$ff_debt,
      na.rm = TRUE
    )
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
  filter(.data$year %in% .env$relevant_years)

for (scenario_source in unique(scenarios_long$scenario_source)) {
  filename <- paste0("equity_abcd_scenario_", scenario_source, ".rds")
  scenarios_long_source <- filter(scenarios_long, .data$scenario_source == .env$scenario_source)
  log_info(paste0("Formatting and saving ", filename, "... "))
  pacta.data.preparation:::dataprep_abcd_scen_connection(
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
  filter(.data$year %in% .env$relevant_years)

for (scenario_source in unique(scenarios_long$scenario_source)) {
  filename <- paste0("bonds_abcd_scenario_", scenario_source, ".rds")
  scenarios_long_source <- filter(scenarios_long, .data$scenario_source == .env$scenario_source)
  log_info(paste0("Formatting and saving ", filename, "... "))
  pacta.data.preparation:::dataprep_abcd_scen_connection(
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


# manifests of input and output file -------------------------------------------

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
      factset_isin_to_fund_table_path = factset_isin_to_fund_table_path,
      ishares_indices_bonds_data_path = ishares_indices_bonds_data_path,
      ishares_indices_equity_data_path = ishares_indices_equity_data_path
    ),
    factset_database = list(
      dbname = dbname,
      host = host,
      username = username
    ),
    timestamps = list(
      imf_quarter_timestamp = imf_quarter_timestamp,
      factset_data_timestamp = factset_data_timestamp,
      pacta_financial_timestamp = pacta_financial_timestamp,
      indices_timestamp = indices_timestamp
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
    years  = list(
      market_share_target_reference_year = market_share_target_reference_year,
      time_horizon = time_horizon,
      additional_year = additional_year,
      relevant_years = relevant_years
    ),
    technologies = list(
      zero_emission_factor_techs = zero_emission_factor_techs,
      green_techs = green_techs,
      tech_exclude = tech_exclude
    ),
    indices = list(
      bonds_indices_urls = bonds_indices_urls,
      equity_indices_urls = equity_indices_urls
    ),
    update_factset = update_factset
  )

pacta.data.preparation:::write_manifest(
  path = file.path(data_prep_outputs_path, "manifest.json"),
  parameters = parameters,
  data_prep_inputs_path = data_prep_inputs_path,
  data_prep_outputs_path = data_prep_outputs_path
)


# ------------------------------------------------------------------------------

log_info("PACTA Data Preparation Complete.")