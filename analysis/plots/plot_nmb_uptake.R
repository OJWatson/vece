library(tidyverse)
library(ggrepel)

# Load results
scenario_results_all <- readRDS("analysis/data-derived/scenario_results_policies.rds")

# Step 1: Calculate NMB with CI
nmb_by_policy <- scenario_results_all %>%
  mutate(
    total_loss_averted = mortality_loss + hosp_prod_loss + symptomatic_loss +
      long_covid_loss + informal_care_loss,
    nmb = total_loss_averted - vaccine_cost
  ) %>%
  group_by(policy_name, vaccine_uptake) %>%
  summarise(
    mean_nmb = mean(nmb) / 1e6,  # £ millions
    lower_ci = quantile(nmb, 0.025) / 1e6,
    upper_ci = quantile(nmb, 0.975) / 1e6,
    .groups = "drop"
  )

# Step 2: Plot NMB with CI ribbons
p <- ggplot(nmb_by_policy, aes(x = vaccine_uptake, y = mean_nmb, color = policy_name, fill = policy_name)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, color = NA) +  # shaded CI
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = round(mean_nmb, 0)), size = 3, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Net Monetary Benefit (NMB) by Vaccine Uptake Level",
    subtitle = "Values in £ millions with 95% confidence interval (shaded); dashed line = £0",
    x = "Vaccine Uptake",
    y = "NMB (£ millions)",
    color = "Policy",
    fill = "Policy"
  ) +
  theme_minimal(base_size = 13)

# Show plot in RStudio Plots pane
print(p)

# Save
ggsave("analysis/plots/plot_nmb_uptake_ci.png", plot = p, width = 10, height = 6)


