# Main analysis script: runs all scenarios using parameter grid
# R/run_scenarios.R

library(tidyverse)
library(purrr)
devtools::load_all()


# --- Load modular functions ---
# source("R/create_UK_data.R")            # Static population and risk inputs
# source("R/calculate_disease_burden.R")  # Scenario-specific infections & outcomes
# source("R/loss_strategy.R")              # Productivity and mortality losses
# source("R/cost_strategy.R")              # Vaccine costs

# read in ifrs
ifr_tbl <- read.csv("analysis/data-derived/ifhr_table.csv")

# --- Step 1: Define scenario grid with best/central/worst cases ---
scenario_grid <- expand.grid(
  vaccine_uptake       = c(0.80, 0.90, 0.95),   # worst, central, best
  infection_rate       = c(0.0313, 0.0361, 0.0413), # lower, central, upper
  vaccine_efficacy     = c("lower", "central", "upper")
)

# --- Step 2: Define shared fixed parameters ---
params <- list(
  los = 8,
  cost_per_day = 933,
  post_hosp_days = 8,
  symptomatic_days_lost = 1.5,
  long_covid_prob = 0.033,
  long_covid_short_days = 12*7,
  long_covid_medium_days = 18*7,
  long_covid_long_days = 24*7,
  care_days = 3,
  cost_vaccine_total = 25,
  ifr_ihrs = ifr_tbl
)

# NOTE TO ADD IN EMPLYMENT %s for the UK


# --- Step 4: JCVI Spring 2025 Policy (75+ only, excluding care home and immunosuppressed) ---
scenario_results_jcvi <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate, vaccine_efficacy) {

    # --- Step 1: Identify correct vaccine efficacy for scenario ---
    ve_hosp <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_h_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_h,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_h_ub
    )
    ve_death <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_d_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_d,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_d_ub
    )

    # --- Step 2: Create base UK population data ---
    uk_base <- create_country_data(iso3c = "GBR", ifr = params$ifr_ihrs$ifr_naive, ihr = params$ifr_ihrs$ihr_naive)
    uk_base$ve_hosp <- ve_hosp
    uk_base$ve_death <- ve_death
    uk_pop <- uk_base %>%
      mutate(vaccine_uptake = vaccine_uptake) %>%
      mutate(vaccine_uptake = replace(vaccine_uptake, !(age_group %in% c("75-80", "80+")), 0)) %>% # JCVI proxy: 75+ only
      ungroup()

    # --- Step 3: Calculate burden ---
    df_burden <- calculate_disease_burden(
      df = uk_pop,
      infection_rate = infection_rate,
      vaccine_uptake = uk_pop$vaccine_uptake,
      ve_hosp = uk_pop$ve_hosp,
      ve_death = uk_pop$ve_death,
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake, params$cost_vaccine_total)

    # already calculated in your loss function
    # hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      policy_name = "JCVI Spring 2025 (75+ only)",
      vaccine_uptake,
      infection_rate,
      vaccine_efficacy,
      hospital_cost = loss$hospital_medical_cost,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 5: Universal Policy (All Ages) ---
scenario_results_universal <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate, vaccine_efficacy) {

    # --- Step 1: Identify correct vaccine efficacy for scenario ---
    ve_hosp <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_h_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_h,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_h_ub
    )
    ve_death <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_d_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_d,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_d_ub
    )

    # --- Step 2: Create base UK population data ---
    uk_base <- create_country_data(iso3c = "GBR", ifr = params$ifr_ihrs$ifr_naive, ihr = params$ifr_ihrs$ihr_naive)
    uk_base$ve_hosp <- ve_hosp
    uk_base$ve_death <- ve_death
    uk_pop <- uk_base %>%
      mutate(vaccine_uptake = vaccine_uptake) %>%
        ungroup()

    df_burden <- calculate_disease_burden(
      df = uk_pop,  # no filter, all age groups
      infection_rate = infection_rate,
      vaccine_uptake = uk_pop$vaccine_uptake,
      ve_hosp = uk_pop$ve_hosp,
      ve_death = uk_pop$ve_death,
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake, params$cost_vaccine_total)

    # already calculated in your loss function
    # hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      policy_name = "Universal (All Ages)",
      vaccine_uptake,
      infection_rate,
      vaccine_efficacy,
      hospital_cost = loss$hospital_medical_cost,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 6: All Adults Policy (18+) ---
scenario_results_adults <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate, vaccine_efficacy) {

    # --- Step 1: Identify correct vaccine efficacy for scenario ---
    ve_hosp <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_h_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_h,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_h_ub
    )
    ve_death <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_d_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_d,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_d_ub
    )

    # --- Step 2: Create base UK population data ---
    uk_base <- create_country_data(iso3c = "GBR", ifr = params$ifr_ihrs$ifr_naive, ihr = params$ifr_ihrs$ihr_naive)
    uk_base$ve_hosp <- ve_hosp
    uk_base$ve_death <- ve_death
    uk_pop <-  uk_base %>%
      mutate(vaccine_uptake = vaccine_uptake) %>%
      mutate(vaccine_uptake = replace(vaccine_uptake, age_group %in% c("0-5", "5-10", "10-15", "15-20"), 0)) %>%
      ungroup()

    df_burden <- calculate_disease_burden(
      df = uk_pop,
      infection_rate = infection_rate,
      vaccine_uptake = uk_pop$vaccine_uptake,
      ve_hosp = uk_pop$ve_hosp,
      ve_death = uk_pop$ve_death,
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake, params$cost_vaccine_total)

    # already calculated in your loss function
    # hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      policy_name = "All Adults (18+)",
      vaccine_uptake,
      infection_rate,
      vaccine_efficacy,
      hospital_cost = loss$hospital_medical_cost,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 7: No Vaccine ---
scenario_results_novaccine <- pmap_dfr(
  scenario_grid %>% mutate(vaccine_uptake = 0) %>% unique,
  function(vaccine_uptake, infection_rate, vaccine_efficacy) {

    vaccine_uptake <- 0

    # --- Step 1: Identify correct vaccine efficacy for scenario ---
    ve_hosp <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_h_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_h,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_h_ub
    )
    ve_death <- case_when(
      vaccine_efficacy == "lower" ~ params$ifr_ihrs$ve_d_lb,
      vaccine_efficacy == "central" ~ params$ifr_ihrs$ve_d,
      vaccine_efficacy == "upper" ~ params$ifr_ihrs$ve_d_ub
    )

    # --- Step 2: Create base UK population data ---
    uk_base <- create_country_data(iso3c = "GBR", ifr = params$ifr_ihrs$ifr_naive, ihr = params$ifr_ihrs$ihr_naive)
    uk_base$ve_hosp <- ve_hosp
    uk_base$ve_death <- ve_death
    uk_pop <-  uk_base %>%
      mutate(vaccine_uptake = vaccine_uptake) %>%
      mutate(vaccine_uptake = replace(vaccine_uptake, age_group %in% c("0-5", "5-10", "10-15", "15-20"), 0)) %>%
      ungroup()

    df_burden <- calculate_disease_burden(
      df = uk_pop,
      infection_rate = infection_rate,
      vaccine_uptake = uk_pop$vaccine_uptake,
      ve_hosp = uk_pop$ve_hosp,
      ve_death = uk_pop$ve_death,
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake, params$cost_vaccine_total)

    # already calculated in your loss function
    # hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      policy_name = "No Vaccination",
      vaccine_uptake,
      infection_rate,
      vaccine_efficacy,
      hospital_cost = loss$hospital_medical_cost,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 8: Combine All Policies ---
scenario_results_all <- bind_rows(
  scenario_results_jcvi,
  scenario_results_universal,
  scenario_results_adults,
  scenario_results_novaccine
)

# --- Step 8: Save Combined Results ---
write_csv(scenario_results_all, "analysis/tables/scenario_results_policies.csv")
saveRDS(scenario_results_all, "analysis/data-derived/scenario_results_policies.rds")

# --- Optional: QA Preview ---
print(scenario_results_all)
