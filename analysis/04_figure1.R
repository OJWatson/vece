# Makes Figure 1

library(tidyverse)

# --- Load pre-computed policy results ---
scenario_results_all <- readRDS("analysis/data-derived/scenario_results_policies.rds")

# --- Step 1: Summarise across scenarios by policy ---
policy_summary <- scenario_results_all %>%
  filter(vaccine_efficacy == "central" & vaccine_uptake %in% c(0,0.9) & infection_rate == 0.0361) %>%
  group_by(policy_name) %>%
  mutate(across(where(is.numeric), ~ . / 1e9))  # convert to £ billions

# --- Step 2: Reshape for stacked plotting ---
loss_long <- policy_summary %>%
  pivot_longer(
    cols = c(mortality_loss, hospital_cost, hosp_prod_loss, symptomatic_loss, long_covid_loss, informal_care_loss),
    names_to = "loss_type",
    values_to = "loss_value"
  )

# --- Step 3: Main stacked bar plot with vaccine cost ---
gg1 <- ggplot(loss_long, aes(x = fct_reorder(policy_name, vaccine_cost), y = loss_value, fill = fct_reorder(loss_type, loss_value))) +
  geom_col(position = position_dodge(width = 0.4), width = 0.4) +

  # Add adjacent vaccine cost bar
  geom_col(data = policy_summary,
           aes(x = policy_name, y = vaccine_cost, fill="Vaccine Cost"),
           width = 0.08,
           position = position_nudge(x = 0.3),
           inherit.aes = FALSE) +

  # Custom fill colours and labels including vaccine cost
  scale_fill_manual(
    values = c(
      mortality_loss = MetBrewer::MetPalettes$Klimt[[1]][1],
      hospital_cost = MetBrewer::MetPalettes$Klimt[[1]][7],
      hosp_prod_loss = MetBrewer::MetPalettes$Klimt[[1]][2],
      symptomatic_loss = MetBrewer::MetPalettes$Klimt[[1]][4],
      long_covid_loss = MetBrewer::MetPalettes$Klimt[[1]][5],
      informal_care_loss = MetBrewer::MetPalettes$Klimt[[1]][6],
      `Vaccine Cost` = MetBrewer::MetPalettes$Klimt[[1]][3]
    ),
    breaks = c("mortality_loss", "hospital_cost", "hosp_prod_loss", "symptomatic_loss", "long_covid_loss", "informal_care_loss", "Vaccine Cost")[c(3,6,4,5,1,2,7)],
    labels = c("Mortality Loss", "Hospital Treatment Cost", "Hospital Productivity Loss", "Symptomatic Loss", "Long COVID Loss", "Informal Care Loss", "Vaccine Cost")[c(3,6,4,5,1,2,7)]
  ) +

  labs(
    x = "\nVaccination Policy",
    y = "Cost / Loss (£ billions)\n",
    fill = "Cost / Loss Component\n"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.key.spacing.y = unit(0.5, "line")) +
  scale_y_sqrt(expand = expand_scale(mult = c(0, 0.05)), breaks = c(0.25,1,2,4,8, 16))
gg1

save_figs("figure1", gg1, 15, height = 8)
