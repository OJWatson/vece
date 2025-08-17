# 07_run_vsly_sweep.R
# Runs scenarios across a range of VSLY scaling factors and saves results

library(tidyverse)
library(purrr)
devtools::load_all()

# --- Load input data ---
ifr_tbl <- read.csv("analysis/data-derived/ifhr_table.csv")
employment_tbl <- read.csv("analysis/data-derived/uk_employment_rate_2024.csv")

# --- Define VSLY scaling factors ---
vsly_factors <- exp(seq(-3, 0.5, 0.1))  # from 0.05x to 1.65x VSLY

# --- Define scenario grid ---
scenario_grid_vsly <- expand.grid(
  vaccine_uptake = c(0.80, 0.90, 0.95),
  infection_rate = c(0.0361),           # central infection rate
  vaccine_efficacy = c("central"),      # central efficacy
  vsly_factor = vsly_factors
)

# --- Shared base parameters ---
params <- list(
  los = 8,
  cost_per_day = 933,
  post_hosp_days = 8,
  symptomatic_days_lost = 1.5,
  long_covid_prob = 0.033,
  long_covid_short_days = 12 * 7,
  long_covid_medium_days = 18 * 7,
  long_covid_long_days = 24 * 7,
  care_days = 3,
  cost_vaccine_total = 25,
  ifr_ihrs = ifr_tbl
)

# --- Policy definitions ---
policy_list <- list(
  list(name = "JCVI Spring 2025 (75+ only)",
       filter = function(df, uptake) {
         df %>% mutate(vaccine_uptake = uptake) %>%
         mutate(vaccine_uptake = replace(vaccine_uptake, !(age_group %in% c("75-80", "80+")), 0))
         }),
  list(name = "Over 65s",
       filter = function(df, uptake) {
         df %>% mutate(vaccine_uptake = uptake) %>%
           mutate(vaccine_uptake = replace(vaccine_uptake, !(age_group %in% c("65-70","70-75","75-80", "80+")), 0))
       }),
  list(name = "All Adults (18+)",
       filter = function(df, uptake) {
         df %>% mutate(vaccine_uptake = uptake) %>%
           mutate(vaccine_uptake = replace(vaccine_uptake, age_group %in% c("0-5","5-10","10-15","15-20"), 0))
         }),
  list(name = "No Vaccination",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = 0))
)

# --- Run VSLY sweep scenarios ---
results_vsly <- pmap_dfr(
  scenario_grid_vsly,
  function(vaccine_uptake, infection_rate, vaccine_efficacy, vsly_factor) {

    ve_hosp <- params$ifr_ihrs$ve_h
    ve_death <- params$ifr_ihrs$ve_d

    map_dfr(policy_list, function(pol) {
      uk_base <- create_country_data(iso3c = "GBR",
                                     ifr = params$ifr_ihrs$ifr_naive,
                                     ihr = params$ifr_ihrs$ihr_naive) %>%
        mutate(employment_rate_percent = employment_tbl$employment_rate_percent) %>%
        mutate(employment_rate = employment_rate_percent / 100)

      uk_base$ve_hosp <- ve_hosp
      uk_base$ve_death <- ve_death

      uk_pop <- pol$filter(uk_base, vaccine_uptake) %>% ungroup()

      df_burden <- calculate_disease_burden(
        df = uk_pop,
        infection_rate = infection_rate,
        vaccine_uptake = uk_pop$vaccine_uptake,
        ve_hosp = uk_pop$ve_hosp,
        ve_death = uk_pop$ve_death
      )

      df_burden <- df_burden %>%
        mutate(vsly = vsly * vsly_factor)

      loss <- calculate_losses(df_burden, params)
      vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake, params$cost_vaccine_total)

      tibble(
        policy_name = pol$name,
        vaccine_uptake,
        infection_rate,
        vaccine_efficacy,
        vsly_factor,
        hospital_cost = loss$hospital_medical_cost,
        vaccine_cost = vax_cost,
        symptomatic_loss = loss$symptomatic_loss,
        long_covid_loss = loss$long_covid_loss,
        mortality_loss = loss$mortality_loss,
        informal_care_loss = loss$informal_care_loss,
        hosp_prod_loss = loss$hosp_prod_loss,
        total_productivity_loss = loss$total_loss,
        total_loss = loss$total_loss
      )
    })
  }
)

# --- Save results ---
saveRDS(results_vsly, "analysis/data-derived/scenario_results_vsly.rds")
write_csv(results_vsly, "analysis/tables/scenario_results_vsly.csv")
cat("✅ VSLY sweep complete. Results saved to analysis/data-derived/scenario_results_vsly.rds\n")

# --- Step 4: Load saved results for plotting ---
results_vsly <- readRDS("analysis/data-derived/scenario_results_vsly.rds")

# --- Step 5: Calculate relative NMB (baseline = No Vaccination) ---
baseline_vsly <- results_vsly %>%
  filter(policy_name == "No Vaccination") %>%
  select(vaccine_uptake, vsly_factor, no_vacc_loss = total_loss)

df_vsly <- results_vsly %>% filter(policy_name != "No Vaccination") %>%
  left_join(baseline_vsly, by = c("vaccine_uptake", "vsly_factor")) %>%
  mutate(nmb = (no_vacc_loss - total_loss) - vaccine_cost) %>%
  group_by(policy_name, vaccine_uptake, vsly_factor) %>%
  summarise(mean_rel_nmb = mean(nmb), .groups = "drop") %>%
  mutate(vacuptext = paste0("Vaccine Uptake = ", scales::percent(vaccine_uptake)))

# --- Step 6: Plot Figure 4 (relative NMB) ---
fig4 <- ggplot(df_vsly, aes(x = vsly_factor, y = mean_rel_nmb/1e9, color = policy_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geomtextpath::geom_textvline(xintercept = 20000/vsl_fn()$vsly[1], label = "Keeling et al. VSLY", inherit.aes = FALSE, size = 5, family = "Helvetica") +
  facet_wrap(~ vacuptext) +
  labs(
    x = "\nVSLY Scaling Factor",
    y = "Net Monetary Benefit \n(£ billions, relative to No Vaccination)\n",
    color = "Vaccination Policy"
  ) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(strip.text = element_text(size = 16), legend.position = "top", axis.text.x = element_text(size = 11)) +
  scale_y_log10() +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 1.5), labels = scales::percent_format())
fig4

# --- Save as PNG and PDF ---
ggsave("analysis/plots/figure4_vsly.png", fig4, width = 14, height = 6.5, dpi = 300)
ggsave("analysis/plots/figure4_vsly.pdf", fig4, width = 16, height = 6)

cat("✅ Figure 4 (VSLY sensitivity) saved to analysis/plots/figure4_vsly.[png/pdf]\n")
