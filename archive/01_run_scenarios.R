library(tidyverse)
source("R/calculate_productivity_loss.R")
uk_pop_df <- readRDS("data-derived/uk_pop_df.rds")
scenario_df <- expand.grid(
  infection_rate = c(0.01, 0.015, 0.02),
  vaccine_uptake = c(0.5, 0.7, 0.9),
  efficacy_symptomatic = c(0.5, 0.6, 0.7)
)
head(scenario_df)       # First few rows
nrow(scenario_df)       # Should return 27
glimpse(scenario_df)    # To inspect all 3 columns
scenario_results <- scenario_df %>%
  mutate(results = pmap(
    list(infection_rate, vaccine_uptake, efficacy_symptomatic),
    function(infect, uptake, eff) {
      calculate_productivity_loss(
        df = uk_pop_df,
        infection_rate = infect,
        vaccine_uptake = uptake,
        efficacy_symptomatic = eff
      ) %>%
        summarise(
          Mortality_Loss = sum(mortality_loss, na.rm = TRUE),
          Hospital_Burden = sum(hospital_burden, na.rm = TRUE),
          Hospital_Productivity_Loss = sum(hospital_productivity_loss, na.rm = TRUE),
          Symptomatic_Loss = sum(symptomatic_loss, na.rm = TRUE),
          Long_COVID_Burden = sum(long_covid_burden, na.rm = TRUE),
          Informal_Caregiving_Burden = sum(informal_care_burden, na.rm = TRUE),
          Vaccination_Cost = sum(vaccination_cost, na.rm = TRUE)
        )
    }
  ))
