################################################################################
# The purpose of this script is to look into:
# (a) the overall fare revenue for PT and DRT for each scanrio, and to compare
#     to the baseline
# (b) the operating costs DRT, and to compare with the change in fare revenue
################################################################################


# For each trip: we want a value for PT fare,
# We also could look at additional PT revenue due to introduction of DRT, and


library(tidyverse)
library(gt)



# Set up a list of scenarios and fleet sizes to read in (file directories should exist)
scenarios <- c("zones", "all", "innerBUA")
# fleet_sizes <- c(100, 200, 500, 1000)
fleet_sizes <- c(100, 200, 500)


# Function to read and process a file and add identifier column
read_and_process <- function(scenario, fleet_size, file_name) {
  # Construct the file path
  file_path <- paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/", file_name, ".csv")

  # Check if file exists
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)  # Safe fail
  }

  # Print status
  print(paste("Reading file:", file_path))
  # Read the file
  data <- read_delim(file_path, delim = ";")
  # Add identifiers
  data_filtered <- data %>%
    mutate(scenario = scenario, fleet_size = fleet_size)

  # # We are only interested in trips that have a drt leg. Let's do this pre-processing step here
  # # instead of on the whole dataset
  # trips_with_drt = data %>%
  #   group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  #   # keep only the groups where "drt" exists in one of the rows in the mode column
  #   filter(any(str_detect(mode, "drt"))) %>%
  #   ungroup()

  return(data_filtered)
}


# Create a data frame of all combinations of scenarios and fleet sizes to read in
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

# ---------- Load in all  trips ---------- #

# Use purrr::pmap_dfr to read and process each combination. All dfs are binded together
legs <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_legs")
})

# convert any mode that contains "drt" to "drt"
legs <- legs %>%
  mutate(mode = case_when(
    str_detect(mode, "drt") ~ "drt",
    TRUE ~ mode
  ))



###########################################################################
# ------------------------ FARE CALCULATIONS -----------------------------#
###########################################################################

###################################
# STEP 1: PT FARE FOR BASE SCENARIO
###################################

# price of a single bus ticket
single_ticket = 3

legs_base_scenario = read_delim("../scenarios/basic/sample_1.00/eqasim_legs.csv")

legs_base_scenario = tidytable::as_tidytable(legs_base_scenario)

legs_pt_fare_base = legs_base_scenario %>%
  tidytable::group_by(person_id, person_trip_id) %>%
  # add a row that indicates number of times pt is in the mode column
  tidytable::summarise(
    trips_pt = sum(mode == "pt")) %>%
  tidytable::ungroup() %>%
  # total fare
  tidytable::mutate(pt_fare = trips_pt * single_ticket)

# total fare
pt_fare_base_total = sum(legs_pt_fare_base$pt_fare)
pt_trips_base_total = sum(legs_pt_fare_base$trips_pt)


###################################
# STEP 2: PT FARE FOR ALL DRT SCENARIOS
###################################


# Calculate number of PT and DRT vehicles used per unique trip (and number of drt_feeder trips)
legs = tidytable::as_tidytable(legs)

legs_pt_drt = legs %>%
  tidytable::group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  # add a row that indicates number of times pt is in the mode column
  tidytable::summarise(
    trips_pt = sum(mode == "pt"),
    trips_drt = sum(mode == "drt"),
    # check if the drt for this trip is a feeder or not
    drt_is_feeder = tidytable::if_else(any(mode == "pt") & any(mode == "drt"), "YES", "NO"),
    # count number of feeder trips
    trips_drt_feeder = tidytable::if_else(any(mode == "pt") & any(mode == "drt"),
                               sum(mode == "drt"),
                               0L)) %>%
  tidytable::ungroup()


# --- Fare calculation

# 1. calculate fare of each trip
legs_pt_drt = legs_pt_drt %>%
  tidytable::mutate(pt_fare = tidytable::case_when(
    # If there are PT trips (the drt component is free so we don't need to look at it)
    trips_pt > 0 ~ trips_pt * single_ticket,
    # if there are no pt trips, then fare is based on number of drt trips
    trips_pt == 0 ~ trips_drt * single_ticket,
    # all cases should already be covered
    TRUE ~ 0))

# 2. calculate total fare per scenario
legs_pt_drt_fare = legs_pt_drt %>%
  tidytable::group_by(scenario, fleet_size) %>%
  tidytable::summarise(
    total_pt_trips = sum(trips_pt),
    total_drt_trips = sum(trips_drt),
    # the proportion of drt trips that are feeders
    proportion_drt_feeder = round(sum(trips_drt_feeder) / sum(trips_drt), 3),
    total_pt_fare = sum(pt_fare)) %>%
  ungroup()

# 3. Compare PT revenue to revenue under base scenario
legs_pt_drt_fare = legs_pt_drt_fare %>%
  # revenue under base scenario
  mutate(total_pt_trips_base = pt_trips_base_total,
         pt_trips_difference = total_pt_trips - total_pt_trips_base,
         total_pt_fare_base = pt_fare_base_total,
         fare_difference = total_pt_fare - total_pt_fare_base)

# save the output as a csv
write_csv(legs_pt_drt_fare, "plots/cost_and_fares/total_pt_revenue_per_scenario.csv")


# ----------- Summary table

summary_table <- legs_pt_drt_fare %>%
    mutate(scenario = case_when(
    scenario == "zones" ~ "drtNE|NW",
    scenario == "innerBUA" ~ "drtInner",
    scenario == "all" ~ "drtAll",
    TRUE ~ as.character(scenario)
  )) %>%
  select(scenario, fleet_size, total_drt_trips, proportion_drt_feeder, pt_trips_difference, fare_difference) %>%
  gt(groupname_col = "scenario") %>%
  # Add a title and subtitle
  tab_header(
    title = "Impact of DRT Scenarios on Public Transport System",
    subtitle = "Comparison of DRT uptake, feeder performance, and impact on PT trips and revenue"
  ) %>%
  # Relabel columns for clarity
  cols_label(
    fleet_size = "Fleet Size",
    total_drt_trips = "DRT Trips",
    proportion_drt_feeder = "Feeder Proportion (of Total DRT)",
    pt_trips_difference = "Change in PT Trips",
    fare_difference = "Change in PT Fare (£)"
  ) %>%
  # Format numbers
  fmt_number(
    columns = c(fleet_size, total_drt_trips, pt_trips_difference),
    decimals = 0,
    use_seps = TRUE # Use comma separators
  ) %>%
  fmt_percent(
    columns = proportion_drt_feeder,
    decimals = 1
  ) %>%
  fmt_currency(
    columns = fare_difference,
    currency = "GBP",
    use_seps = TRUE
  ) %>%
  # Add a source note
  tab_source_note(
    source_note = "'Change' columns compare to the no-DRT calibrated base scenario. No. of PT trips in the base scenario: 169,822"
  )

# Print the table
print(summary_table)


summary_table %>% as_latex()



###########################################################################
# ------------------------ OPERATING COSTS -----------------------------#
###########################################################################


# Use purrr::pmap_dfr to read and process each combination. All dfs are binded together
drt_movements <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_drt_vehicle_movements")
})

drt_vkm_total = drt_movements %>%
  group_by(scenario, fleet_size) %>%
  # get distance in km
  summarise(total_daily_vkt = sum(distance / 1000)) %>%
  ungroup()

# ==============================================================================
# STEP 1: DEFINE INPUT PARAMETERS
# ==============================================================================

# Parameters based on
   #  Becker et al. (2020): Impact of vehicle automation and electric propulsion on production costs for mobility services worldwide
   #  Bösch et al. (2018) Cost-based analysis of autonomous mobility services
   #  Zwick et al. (2022) Shifts in perspective: Operational aspects in (non-)autonomous ride-pooling simulations


# --- 1a. Cost Parameter Scenarios ---
# Define parameters as single values or vectors for sensitivity analysis.
# The script will test every combination of the values provided in these vectors.

# Labour Costs (only used for conventional fleets)
# fully_loaded_hourly_wage <- c(15.00, 16.80, 18.50) # Low, Medium (base), High wage
fully_loaded_hourly_wage <- c(16.80) # Medium (base)


# Energy Costs
# diesel_price_per_l <- c(1.40, 1.55, 1.70)          # Low, Medium (base), High fuel price
diesel_price_per_l <- c(1.55)          # Medium (base)

# electricity_price_per_kwh <- c(0.18, 0.22, 0.26)   # Low, Medium (base), High electricity price
electricity_price_per_kwh <- c(0.22)   # Medium (base)


# Other Operational Costs
# maintenance_cost_per_km <- c(0.07, 0.08, 0.10)     # Low, Medium (base), High maintenance cost
maintenance_cost_per_km <- c(0.08)     # Medium (base)

overhead_cost_per_vehicle_daily <- 22
cleaning_cost_per_vehicle_daily <- 15
avg_driver_hours_per_vehicle_daily <- 12

# Vehicle-specific parameters
ice_consumption_l_per_km <- 0.10
bev_consumption_kwh_per_km <- 0.25


# ==============================================================================
# STEP 2: COST CALCULATION FUNCTION
# ==============================================================================

calculate_operational_costs <- function(params) {
  # Labour Cost
  if (params$fleet_type == "Autonomous") {
    total_labour_cost <- 0
  } else {
    total_labour_cost <- params$fleet_size * params$avg_driver_hours_per_vehicle_daily * params$fully_loaded_hourly_wage
  }

  # Energy Cost
  if (params$propulsion_type == "ICE") {
    total_energy_cost <- params$total_daily_vkt * ice_consumption_l_per_km * params$diesel_price_per_l
  } else {
    total_energy_cost <- params$total_daily_vkt * bev_consumption_kwh_per_km * params$electricity_price_per_kwh
  }

  # Other Costs
  total_maintenance_cost <- params$total_daily_vkt * params$maintenance_cost_per_km
  total_overhead_cost <- params$fleet_size * params$overhead_cost_per_vehicle_daily
  total_cleaning_cost <- params$fleet_size * params$cleaning_cost_per_vehicle_daily

  # Total Operational Cost
  total_op_cost <- total_labour_cost + total_energy_cost + total_maintenance_cost + total_overhead_cost + total_cleaning_cost

  return(data.frame(
    total_labour_cost, total_energy_cost, total_maintenance_cost,
    total_overhead_cost, total_cleaning_cost, total_operational_cost = total_op_cost
  ))
}


# ==============================================================================
# STEP 3: PROCESS SCENARIOS AND GENERATE RESULTS
# ==============================================================================

# 1. Create a data frame with every combination of cost parameters
cost_parameter_scenarios <- tidyr::crossing(
  fully_loaded_hourly_wage,
  diesel_price_per_l,
  electricity_price_per_kwh,
  maintenance_cost_per_km,
  overhead_cost_per_vehicle_daily,
  cleaning_cost_per_vehicle_daily,
  avg_driver_hours_per_vehicle_daily
)

# 2. Define all combinations of fleet and propulsion types
fleet_combinations <- tidyr::crossing(
  fleet_type = c("Conventional", "Autonomous"),
  propulsion_type = c("ICE", "BEV")
)

# 3. Create a comprehensive data frame with all scenarios and all combinations
all_scenarios_df <- tidyr::crossing(
  drt_vkm_total,
  fleet_combinations,
  cost_parameter_scenarios
)

# 4. Apply the cost function to each row (each unique combination)
operating_costs_df <- all_scenarios_df %>%
  group_by(row_number()) %>%
  do(bind_cols(., calculate_operational_costs(.))) %>%
  ungroup() %>%
  select(-`row_number()`)

# save the output as a csv
write_csv(operating_costs_df, "plots/cost_and_fares/total_drt_costs_per_scenario.csv")



# ==============================================================================
# STEP 4: OUTPUT RESULTS
# ==============================================================================


# Calculate and print additional KPIs for easier comparison.
operating_costs_df <- operating_costs_df %>%
  mutate(
    cost_per_km = total_operational_cost / total_daily_vkt,
    cost_per_vehicle = total_operational_cost / fleet_size
  )



###########################################################################
# ------------------------ FARES + COSTS -----------------------------#
###########################################################################


# Join cost to revenue
revenue_and_cost = operating_costs_df %>%
  select(scenario, fleet_size, fleet_type, propulsion_type, total_operational_cost, cost_per_km, cost_per_vehicle) %>%
  left_join(
    legs_pt_drt_fare %>%
      select(scenario, fleet_size, total_drt_trips, proportion_drt_feeder, pt_trips_difference, fare_difference),
    by = c("scenario", "fleet_size"))

# Calculate net cost

revenue_and_cost = revenue_and_cost %>%
  # If fare_difference is positive, then operational cost is reduced (counteracted by additional)
  mutate(net_cost = round(total_operational_cost - fare_difference))

# save the output as a csv
write_csv(revenue_and_cost, "plots/cost_and_fares/overall_revenue_and_cost_per_scenario.csv")


##################
# ---------- Table
##################

summary_table_revenue_cost <- revenue_and_cost %>%
  mutate(scenario = case_when(
    scenario == "zones" ~ "drtNE|NW",
    scenario == "innerBUA" ~ "drtInner",
    scenario == "all" ~ "drtAll",
    TRUE ~ as.character(scenario)
  )) %>%
  # Calculate the overall net cost to the system
  mutate(net_cost = total_operational_cost - fare_difference) %>%
  # Create a combined configuration column for pivoting
  mutate(fleet_config = paste(fleet_type, propulsion_type, sep = " ")) %>%
  select(scenario, fleet_size, fare_difference, fleet_config, total_operational_cost, cost_per_km, net_cost) %>%
  # Pivot the data to make it wider
  pivot_wider(
    names_from = fleet_config,
    values_from = c(total_operational_cost, cost_per_km, net_cost)
  ) %>%
  # Arrange rows for better readability
  arrange(scenario, fleet_size) %>%
  gt(groupname_col = "scenario", rowname_col = "fleet_size") %>%
  # Add a title and subtitle
  tab_header(
    title = "Financial Analysis of DRT Scenarios",
    subtitle = "Comparing PT Revenue Impact with DRT Operational Costs by Fleet Configuration"
  ) %>%
  # Group columns with spanners
  tab_spanner(
    label = "DRT Total Operational Cost (£)",
    columns = starts_with("total_operational_cost")
  ) %>%
  tab_spanner(
    label = "DRT Cost per km (£)",
    columns = starts_with("cost_per_km")
  ) %>%
  tab_spanner(
    label = "Overall Net Cost (£)",
    columns = starts_with("net_cost")
  ) %>%
  # Relabel columns for clarity
  cols_label(
    fare_difference = "Impact on PT Fare (£)",
    `total_operational_cost_Conventional ICE` = "Conv. ICE",
    `total_operational_cost_Conventional BEV` = "Conv. BEV",
    `total_operational_cost_Autonomous ICE` = "Auto. ICE",
    `total_operational_cost_Autonomous BEV` = "Auto. BEV",
    `cost_per_km_Conventional ICE` = "Conv. ICE",
    `cost_per_km_Conventional BEV` = "Conv. BEV",
    `cost_per_km_Autonomous ICE` = "Auto. ICE",
    `cost_per_km_Autonomous BEV` = "Auto. BEV",
    `net_cost_Conventional ICE` = "Conv. ICE",
    `net_cost_Conventional BEV` = "Conv. BEV",
    `net_cost_Autonomous ICE` = "Auto. ICE",
    `net_cost_Autonomous BEV` = "Auto. BEV"
  ) %>%
  # Format numbers and currency
  fmt_currency(
    columns = c(fare_difference, starts_with("total_operational_cost"),
                starts_with("cost_per_km"), starts_with("net_cost")),
    currency = "GBP"
  ) %>%
  # Add a source note
  tab_source_note(
    source_note = "Overall PT revenues and costs. Revenues based on the following fare structure. Bus fare = £3. DRT fare = £3. DRT feeder to PT = £0"
  ) %>%
  tab_footnote(
    footnote = md(
      "<b>Abbreviations</b>: Conv. (Conventional), Auto. (Autonomous), ICE (Internal Combustion Engine), BEV (Battery Electric Vehicle).<br>
      <b>DRT Costs</b>: Calculated using a bottom-up operational model with the following key parameters:<br>
      &bull; Driver Wage (fully loaded): £16.80/hr<br>
      &bull; Diesel: £1.55/L; Electricity: £0.22/kWh<br>
      &bull; Maintenance: £0.08/km<br>
      &bull; Overhead: £22/veh/day; Cleaning: £15/veh/day"
    ),
    locations = cells_column_spanners(spanners = "DRT Total Operational Cost (£)")
  )


# Print the table
print(summary_table_revenue_cost)

summary_table_revenue_cost %>% as_latex()


##################
# -------Visualise
##################

# --- Impact of DRT on overall DRT fares + DRT operating COsts, + Net impact (all on one faceted plot)


# --- Prepare data for plotting ---
# Reshape the data from wide to long format to plot all financial metrics
plotting_data_financial <- revenue_and_cost %>%
  select(scenario, fleet_size, fleet_type, propulsion_type,
         `DRT Operational Cost` = total_operational_cost,
         `DRT impact on overall PT Fare` = fare_difference,
         `Overall Net Cost` = net_cost) %>%
  pivot_longer(
    cols = c("DRT Operational Cost", "DRT impact on overall PT Fare", "Overall Net Cost"),
    names_to = "financial_metric",
    values_to = "value"
  )


ggplot(plotting_data_financial %>%
         mutate(scenario = case_when(
           scenario == "zones" ~ "drtNE|NW",
           scenario == "innerBUA" ~ "drtInner",
           scenario == "all" ~ "drtAll",
           TRUE ~ as.character(scenario))),
       aes(x = scenario, y = value,
           color = factor(fleet_size),
           shape = financial_metric)) +
  geom_point(position = position_dodge(width = 0.7), size = 2.5, alpha = 0.9, stroke = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_grid(fleet_type ~ propulsion_type, labeller = label_both) +
  scale_y_continuous(labels = scales::label_currency(prefix = "£", big.mark = ",")) +
  scale_color_brewer(palette = "Dark2", direction = -1) +
  scale_shape_manual(values = c("DRT Operational Cost" = 16, "DRT impact on overall PT Fare" = 15, "Overall Net Cost" = 8)) + # circle, triangle, square
  labs(
    title = "Overall Financial Impact of DRT Service",
    subtitle = "Comparing DRT Costs and PT Revenue Impacts Across Scenarios",
    x = "DRT Scenario",
    y = "Financial Value (£)",
    color = "Fleet Size",
    shape = "Financial Metric"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


# save plot
ggsave("plots/cost_and_fares/cost_fare_dot_facet_vehilce_type_and_propulsion.png", width = 12)


