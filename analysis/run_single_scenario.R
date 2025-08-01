# Quick test script for running and printing results for a single scenario
# Useful for checking assumptions, logic, or debugging

# Load all core modules
source("R/create_UK_data.R")
source("R/calculate_disease_burden.R")
source("R/loss_strategy.R")
source("R/cost_strategy.R")

# Step 1: Load base data
uk_base <- create_UK_data()

# Step 2: Define single scenario parameters
infection_rate <- 0.2
vaccine_uptake <- 0.8
efficacy_symptomatic <- 0.6

params <- list(
  los = 7,
  cost_per_day = 933,
  hosp_days_lost = 7,
  symptomatic_days_lost = 1.5,
  long_covid_prob = 0.133,
  long_covid_days = 28,
  daily_wage = 128,
  care_days = 3,
  daily_value_care = 128,
  cost_vaccine = 10,
  cost_delivery = 5
)

# Step 3: Run scenario manually
df_burden <- calculate_disease_burden(uk_base, infection_rate, vaccine_uptake, efficacy_symptomatic)
loss <- calculate_losses(df_burden, params)
vax_cost <- calculate_vaccine_cost(df_burden$n, vaccine_uptake, params$cost_vaccine, params$cost_delivery)
hosp_cost <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

# Step 4: Print results
output <- tibble(
  vaccine_uptake = vaccine_uptake,
  infection_rate = infection_rate,
  efficacy_symptomatic = efficacy_symptomatic,
  hospital_cost = hosp_cost,
  vaccine_cost = vax_cost,
  total_productivity_loss = loss$total_loss,
  net_loss = hosp_cost + vax_cost + loss$total_loss
)

print(output)
