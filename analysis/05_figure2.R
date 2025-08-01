# Makes Figure 2

library(tidyverse)
devtools::load_all()

# --- Load pre-computed policy results ---
scenario_results_all <- readRDS("analysis/data-derived/scenario_results_policies.rds")

# --- Step 1: Summarise across scenarios by policy ---
policy_summary <- scenario_results_all %>%
  mutate(across(matches("loss|cost"), ~ . / 1e9))  %>% # convert to £ billions
  group_by(policy_name, vaccine_uptake, infection_rate, vaccine_efficacy) %>%
  summarise(total_loss = hospital_cost + vaccine_cost+symptomatic_loss + long_covid_loss + mortality_loss + informal_care_loss + hosp_prod_loss,
            total_cost = vaccine_cost) %>%
  ungroup()

# --- Step 2: Sort out data for plotting ---
df <- left_join(
  policy_summary %>% filter(vaccine_uptake > 0),
  policy_summary %>% filter(vaccine_uptake == 0) %>%
    select(infection_rate, vaccine_efficacy, no_vacc_loss = total_loss)
) %>%
  mutate(nmb = no_vacc_loss - total_loss) %>%
  pivot_longer(total_loss:total_cost) %>%
  mutate(vaccine_efficacy = as.character(vaccine_efficacy)) %>%
  mutate(vaccine_efficacy = replace(vaccine_efficacy, name == "total_cost", "Total Vaccine Cost")) %>%
  mutate(vaccine_efficacy = case_when(vaccine_efficacy == "central" ~ "Total Loss (Central VE)",
                                      vaccine_efficacy == "lower" ~ "Total Loss (Lower VE)",
                                      vaccine_efficacy == "upper" ~ "Total Loss (Upper VE)",
                                      .default = "Total Vaccine Cost")) %>%
  mutate(vaccine_efficacy = factor(
    vaccine_efficacy,
    levels = c("Total Loss (Lower VE)", "Total Loss (Central VE)", "Total Loss (Upper VE)", "Total Vaccine Cost")
    )) %>%
  mutate(policy_name = factor(policy_name, levels = c("JCVI Spring 2025 (75+ only)", "All Adults (18+)", "Universal (All Ages)"))) %>%
  mutate(infection_rate = paste("Infection Rate = ", scales::percent(infection_rate))) %>%
  mutate(vaccine_uptake = paste("Vaccine Uptake = ", scales::percent(vaccine_uptake)))

# --- Step 3: Plotting ---
gg2 <- df %>%
  ggplot(aes(y = value, x = policy_name, fill = vaccine_efficacy, group = interaction(vaccine_efficacy, name))) +
  geomtextpath::geom_texthline(aes(yintercept = no_vacc_loss), label = "Total Loss with 0% Vaccine Uptake", family = "Helvetica") +
  geom_col(position = position_dodge()) +
  facet_grid(infection_rate ~ vaccine_uptake) +
  theme_bw(12) +
  theme(strip.text = element_text(size = 12)) +
  MetBrewer::scale_fill_met_d(palette_name = "Pissaro", name = "Scenario", override_order = TRUE) +
  labs(
    x = "\nVaccination Policy",
    y = "Cost / Loss (£ billions)\n"
  ) +
  scale_y_sqrt(expand = expand_scale(mult = c(0, 0.05)), breaks = c(0.25,1,2,4,8, 16), limits = c(0,18))

save_figs("figure2", gg2, 17, 8, font_family = "Helvetica")
