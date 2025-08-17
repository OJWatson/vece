# R/visualise_policy_results.R

library(tidyverse)

# --- Load pre-computed policy results ---
scenario_results_all <- readRDS("analysis/data-derived/scenario_results_policies.rds")

# --- Step 1: Summarise across scenarios by policy ---
policy_summary <- scenario_results_all %>%
  group_by(policy_name) %>%
  summarise(
    mortality_loss = mean(mortality_loss),
    hosp_prod_loss = mean(hosp_prod_loss),
    symptomatic_loss = mean(symptomatic_loss),
    long_covid_loss = mean(long_covid_loss),
    informal_care_loss = mean(informal_care_loss),
    vaccine_cost = mean(vaccine_cost),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ . / 1e9))  # convert to £ billions

# --- Step 2: Reshape for stacked plotting ---
loss_long <- policy_summary %>%
  pivot_longer(
    cols = c(mortality_loss, hosp_prod_loss, symptomatic_loss, long_covid_loss, informal_care_loss),
    names_to = "loss_type",
    values_to = "loss_value"
  )

# --- Step 3: Main stacked bar plot with vaccine cost ---
ggplot(loss_long, aes(x = policy_name, y = loss_value, fill = loss_type)) +
  geom_col(position = "stack", width = 0.6) +

  # Add adjacent vaccine cost bar
  geom_col(data = policy_summary,
           aes(x = policy_name, y = vaccine_cost, fill="Vaccine Cost"),
           width = 0.2,
           position = position_nudge(x = 0.4),
           inherit.aes = FALSE) +

  # Custom fill colours and labels including vaccine cost
  scale_fill_manual(
    values = c(
      mortality_loss = "deepskyblue",
      hosp_prod_loss = "tomato",
      symptomatic_loss = "orchid",
      long_covid_loss = "green3",
      informal_care_loss = "goldenrod",
      `Vaccine Cost` = "yellow"
    ),
    breaks = c("mortality_loss", "hosp_prod_loss", "symptomatic_loss", "long_covid_loss", "informal_care_loss", "Vaccine Cost"),
    labels = c("Mortality Loss", "Hospital Productivity Loss", "Symptomatic Loss", "Long COVID Loss", "Informal Care Loss", "Vaccine Cost")
  ) +

  labs(
    title = "Losses and Vaccine Costs Across COVID-19 Vaccination Policies",
    subtitle = "Average loss and cost across all uptake/infection/efficacy scenarios",
    x = "Vaccination Policy",
    y = "Cost / Loss (£ billions)",
    fill = "Loss Component"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
