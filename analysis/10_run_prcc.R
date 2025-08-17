# 10_run_prcc.R
# Performs PRCC analysis on PSA results and generates Figure 6

library(tidyverse)
library(sensitivity)
devtools::load_all()

# --- Load PSA outputs ---
psa_results <- readRDS("analysis/data-derived/scenario_results_psa.rds")
psa_params  <- readRDS("analysis/data-derived/psa_params.rds")

# --- Prepare PRCC dataset ---
df_prcc <- psa_results %>%
  left_join(psa_params, by = "draw") %>%
  group_by(draw, policy_name) %>%
  summarise(mean_nmb = mean(nmb, na.rm = TRUE),
            infection_rate = first(infection_rate),
            cost_vaccine_total = first(cost_vaccine_total),
            los = first(los),
            .groups = "drop")

# --- Run PRCC per policy ---
policies <- unique(df_prcc$policy_name)
prcc_results <- list()

for (pol in policies) {
  df_pol <- df_prcc %>% filter(policy_name == pol)
  X <- df_pol %>% select(infection_rate, cost_vaccine_total, los)
  Y <- df_pol$mean_nmb

  prcc_out <- pcc(as.data.frame(X), Y, rank = TRUE, nboot = 100)

  prcc_results[[pol]] <- tibble(
    policy_name = pol,
    parameter = rownames(prcc_out$PRCC),
    prcc = prcc_out$PRCC[,1],
    lower = prcc_out$PRCC[,2],
    upper = prcc_out$PRCC[,3]
  )
}

prcc_df <- bind_rows(prcc_results)

# --- Save PRCC data ---
saveRDS(prcc_df, "analysis/data-derived/prcc_results.rds")
write_csv(prcc_df, "analysis/tables/prcc_results.csv")
cat("✅ PRCC results saved.\n")

# --- Plot Figure 6 ---
fig6 <- ggplot(prcc_df, aes(x = parameter, y = prcc, fill = parameter)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_wrap(~ policy_name, ncol = 2) +
  labs(
    title = "Figure 6: PRCC of Key Parameters on Net Monetary Benefit (NMB)",
    subtitle = "Partial Rank Correlation Coefficient (PRCC) with 95% CI",
    x = "\nParameter",
    y = "PRCC (Impact on NMB)\n",
    fill = "Parameter"
  ) +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(size = 11))

# --- Save Figure ---
ggsave("analysis/plots/figure6_psa_prcc.png", fig6, width = 10, height = 6, dpi = 300)
ggsave("analysis/plots/figure6_psa_prcc.pdf", fig6, width = 10, height = 6)
cat("✅ Figure 6 saved as PNG and PDF.\n")
