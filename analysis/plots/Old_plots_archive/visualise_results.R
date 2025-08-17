# analysis/visualise_results.R

# --- Load libraries ---
library(tidyverse)
library(scales)

# --- Load results ---
scenario_results <- readRDS("analysis/data-derived/scenario_results.rds")

# --- Step 1: Barplot of total economic losses per scenario ---
# Create a unique scenario ID
scenario_results <- scenario_results %>%
  mutate(scenario_id = paste0("Uptake: ", vaccine_uptake, " | Infection: ", infection_rate))

# Plot barplot
ggplot(scenario_results, aes(x = as.factor(vaccine_uptake), y = total_productivity_loss)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ infection_rate, labeller = label_both) +
  scale_y_continuous(labels = label_number(scale = 1e-9, prefix = "£", suffix = "B")) +
  labs(
    title = "Total Productivity Loss by Vaccine Uptake, Faceted by Infection Rate",
    x = "Vaccine Uptake",
    y = "Total Productivity Loss (£)"
  ) +
  theme_minimal()

ggsave("analysis/tables/barplot_total_prod_loss_by_scenario.png", width = 8, height = 5)

# --- Step 2: Plot loss vs. uptake (line per infection rate) ---
ggplot(scenario_results, aes(x = vaccine_uptake, y = total_productivity_loss, color = as.factor(infection_rate))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Productivity Loss vs Vaccine Uptake by Infection Rate",
    x = "Vaccine Uptake",
    y = "Total Productivity Loss (£)",
    color = "Infection Rate"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B", prefix = "£")) +
  theme_minimal()

ggsave("analysis/tables/lineplot_prod_loss_vs_uptake.png", width = 8, height = 5)

# --- Step 3: Plot Vaccine Cost vs Productivity Loss ---
ggplot(scenario_results, aes(x = vaccine_cost, y = symptomatic_loss, color = factor(infection_rate))) +
  geom_point(size = 3) +
  labs(
    title = "Vaccine Programme Cost vs. Symptomatic Productivity Loss",
    x = "Vaccine Cost (£)",
    y = "Symptomatic Productivity Loss (£)",
    color = "Infection Rate"
  ) +
  scale_x_continuous(labels = label_number(scale = 1e-9, prefix = "£", suffix = "B")) +
  scale_y_continuous(labels = label_number(scale = 1e-9, prefix = "£", suffix = "B")) +
  theme_minimal()

ggsave("analysis/tables/plot_vaccine_cost_vs_symptomatic_loss.png", width = 8, height = 5)


