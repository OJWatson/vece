# Makes Figure 3

library(tidyverse)
devtools::load_all()

# --- Load pre-computed policy results ---
scenario_results_all <- readRDS("analysis/data-derived/scenario_results_policies.rds")

# --- Step 1: Summarise across scenarios by policy ---
policy_summary <- scenario_results_all %>%
  mutate(across(matches("loss|cost"), ~ . / 1e9))  %>% # convert to £ billions
  group_by(policy_name, vaccine_uptake, infection_rate, vaccine_efficacy) %>%
  summarise(total_loss = hospital_cost + vaccine_cost+symptomatic_loss + long_covid_loss  + informal_care_loss + hosp_prod_loss,
            total_cost = vaccine_cost) %>%
  ungroup()

# --- Step 2: Sort out data for plotting ---
df <- left_join(
  policy_summary %>% filter(vaccine_uptake > 0),
  policy_summary %>% filter(vaccine_uptake == 0) %>%
    select(infection_rate, vaccine_efficacy, no_vacc_loss = total_loss)
) %>%
  mutate(nmb = (no_vacc_loss - total_loss) - total_cost) %>%
  group_by(policy_name, vaccine_uptake) %>%
  summarise(
    mean_nmb = mean(nmb),
    lower_ci = quantile(nmb, 0.025),
    upper_ci = quantile(nmb, 0.975),
    .groups = "drop"
  )

## TODO: Rosh to finish
