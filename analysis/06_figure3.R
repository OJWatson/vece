# Makes Figure 3

library(tidyverse)
library(ggrepel)
devtools::load_all()

# --- Load pre-computed policy results ---
scenario_results_all <- readRDS("analysis/data-derived/scenario_results_policies.rds")

# --- Step 1: Summarise across scenarios by policy ---
policy_summary <- scenario_results_all %>%
  mutate(across(matches("loss|cost"), ~ . / 1e9))  %>% # convert to £ billions
  group_by(policy_name, vaccine_uptake, infection_rate, vaccine_efficacy) %>%
  summarise(total_loss = hospital_cost + mortality_loss + symptomatic_loss + long_covid_loss  + informal_care_loss + hosp_prod_loss,
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
## === Step 3: Improved Figure 3 ===

library(ggplot2)
library(ggrepel)   # for label positioning

fig3 <- ggplot(df, aes(x = policy_name, y = mean_nmb, color = factor(vaccine_uptake))) +
  # CI error bars
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2,
                position = position_dodge(width = 0.5)) +

  # Mean NMB points
  geom_point(size = 3, position = position_dodge(width = 0.5)) +

  # Labels for mean NMB
  geom_text_repel(aes(label = round(mean_nmb, 2)),
                  position = position_dodge(width = 0.5),
                  size = 3, show.legend = FALSE) +

  # Horizontal breakeven line
  geomtextpath::geom_labelhline(yintercept = 0, linetype = "dashed", color = "grey40", label = "Breakeven (NMB = £0") +

  # Color palette
  scale_color_manual(values = c("0.8" = "#D55E00", "0.9" = "#0072B2", "0.95" = "#009E73"),
                     name = "Vaccine Uptake") +

  # Labels and theme
  labs(
    x = "\nVaccination Policy",
    y = "Net Monetary Benefit (£ billions)\n"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
fig3

# --- Save ---
ggsave("analysis/plots/figure3_nmb.png", fig3, width = 10, height = 6, dpi = 300)
ggsave("analysis/plots/figure3_nmb.pdf", fig3, width = 10, height = 6)
cat("✅ Figure 3 successfully saved as both PNG and PDF in analysis/plots/ \n")
