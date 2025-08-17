# 08_run_vsly_cost_sweep.R
# Runs scenarios spanning both VSLY scaling and vaccine cost scaling
# Generates Figure 5: Heatmap of NMB vs VSLY Scaling and Vaccine Cost Scaling

library(tidyverse)
library(purrr)
devtools::load_all()

# --- Load input data ---
ifr_tbl <- read.csv("analysis/data-derived/ifhr_table.csv")
employment_tbl <- read.csv("analysis/data-derived/uk_employment_rate_2024.csv")

# --- Define scaling factors ---
vsly_factors <- exp(seq(-3, 0.5, 0.1))  # from 0.05x to 1.65x VSLY
vax_cost_factors <- seq(1, 20, by = 1)   # from 0.1x to 2x vaccine cost

# --- Define scenario grid ---
scenario_grid_vsly_cost <- expand.grid(
  vaccine_uptake = c(0.80, 0.90, 0.95),
  infection_rate = c(0.0361),        # central infection rate
  vaccine_efficacy = c("central"),   # central efficacy
  vsly_factor = vsly_factors,
  cost_factor = vax_cost_factors
)

# --- Shared parameters ---
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
  cost_vaccine_total = 25,    # baseline cost
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
results_vsly_cost <- pmap_dfr(
  scenario_grid_vsly_cost,
  function(vaccine_uptake, infection_rate, vaccine_efficacy, vsly_factor, cost_factor) {

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
      vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake, params$cost_vaccine_total*cost_factor)

      tibble(
        policy_name = pol$name,
        vaccine_uptake,
        infection_rate,
        vaccine_efficacy,
        vsly_factor,
        cost_factor,
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
saveRDS(results_vsly_cost, "analysis/data-derived/scenario_results_vsly_cost.rds")
write_csv(results_vsly_cost, "analysis/tables/scenario_results_vsly_cost.csv")
cat("✅ VSLY + Vaccine Cost sweep complete. Results saved to analysis/data-derived/scenario_results_vsly_cost.rds\n")

# --- Step 4: Calculate relative NMB (baseline = No Vaccination) ---
baseline_vsly_cost <- results_vsly_cost %>%
  filter(policy_name == "No Vaccination") %>%
  select(vaccine_uptake, vsly_factor, cost_factor, no_vacc_loss = total_loss)

df_vsly_cost <- results_vsly_cost %>% filter(policy_name != "No Vaccination") %>%
  left_join(baseline_vsly_cost, by = c("vaccine_uptake", "vsly_factor", "cost_factor")) %>%
  mutate(nmb = (no_vacc_loss - total_loss) - vaccine_cost) %>%
  group_by(policy_name, vaccine_uptake, vsly_factor, cost_factor) %>%
  summarise(mean_rel_nmb = mean(nmb), .groups = "drop") %>%
  mutate(vacuptext = paste0("Vaccine Uptake = ", scales::percent(vaccine_uptake)))

# --- Step 5: Plot Figure 5 (Heatmap of NMB) ---
fig5 <- df_vsly_cost %>%
  filter(policy_name != "No Vaccination") %>%
  ggplot(aes(x = vsly_factor*vsl_fn()$vsly[1], y = cost_factor*25, fill = mean_rel_nmb/1e9)) +
  geom_tile() +
  geom_point(data = . %>% filter(mean_rel_nmb>0), size = 0.5) +
  scale_color_manual(values = c(NA, "black")) +
  scale_fill_gradient2(midpoint = 0, name = "Relative NMB (£ billions)", ) +  # removed log
  facet_grid(policy_name ~ vacuptext) +
  labs(
    x = "\nVSLY Scaling Factor",
    y = "Vaccine Cost Scaling Factor\n"
  ) +
  ylab("Vaccine Cost (£)\n") +
  xlab("\nVSLY (£)") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(size = 11)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_log10(expand = c(0,0))
fig5

# --- Save Figure ---
ggsave("analysis/plots/figure5_vsly_cost_heatmap.png", fig5, width = 16, height = 8, dpi = 300)
ggsave("analysis/plots/figure5_vsly_cost_heatmap.pdf", fig5, width = 16, height = 8)

cat("✅ Figure 5 (VSLY + Vaccine Cost sensitivity) saved to analysis/plots/figure5_vsly_cost_heatmap.[png/pdf]\n")
