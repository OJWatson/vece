# 09_run_psa_parallel_safe.R
# Parallelized PSA with chunking, disk-saving, and memory cleanup
library(tidyverse)
library(furrr)
devtools::load_all()

# --- Load input data ---
ifr_tbl <- read.csv("analysis/data-derived/ifhr_table.csv")
employment_tbl <- read.csv("analysis/data-derived/uk_employment_rate_2024.csv")

# --- Define vsl_fn (needed in workers) ---
vsl_fn <- function(iso = "GBR", vsl_path = "analysis/data-derived/vsly.rds") {
  readRDS(vsl_path) %>%
    filter(iso3c == iso) %>%
    select(age_group, n = Ng, le = lg, vsl, vsly, iso3c)
}

# --- PSA parameters ---
set.seed(123)
n_draws <- 100                     # total draws
chunk_size <- 5                    # small batches
vsly_factors <- seq(0.1, 2, by = 0.1)
vax_cost_factors <- seq(0.1, 2, by = 0.1)

# --- Parameter draws ---
psa_params <- map_dfr(1:n_draws, function(i) {
  tibble(
    draw = i,
    infection_rate = rnorm(1, 0.0361, 0.005),
    cost_vaccine_total = rnorm(1, 25, 5),
    los = rnorm(1, 8, 1)
  )
})

# --- Scenario grid ---
scenario_grid_psa <- expand.grid(
  draw = 1:n_draws,
  vaccine_uptake = c(0.80, 0.90, 0.95),
  vsly_factor = vsly_factors,
  cost_factor = vax_cost_factors
)

# --- Fixed parameters ---
params_fixed <- list(
  cost_per_day = 933,
  post_hosp_days = 8,
  symptomatic_days_lost = 1.5,
  long_covid_prob = 0.033,
  long_covid_short_days = 12 * 7,
  long_covid_medium_days = 18 * 7,
  long_covid_long_days = 24 * 7,
  care_days = 3,
  ifr_ihrs = ifr_tbl
)

# --- Policy definitions ---
policy_list <- list(
  list(name = "JCVI Spring 2025 (75+ only)",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = replace(uptake, !(age_group %in% c("75-80","80+")), 0))),
  list(name = "Universal (All Ages)",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = uptake)),
  list(name = "All Adults (18+)",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = replace(uptake, age_group %in% c("0-5","5-10","10-15","15-20"), 0))),
  list(name = "No Vaccination",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = 0))
)

# --- Load required functions ---
source("R/create_UK_data.R")
source("R/calculate_disease_burden.R")
source("R/loss_strategy.R")
source("R/cost_strategy.R")

# ✅ Parallel settings
plan(multisession, workers = max(2, parallel::detectCores() - 1))

# --- Split draws into chunks ---
draw_chunks <- split(1:n_draws, ceiling(seq_along(1:n_draws) / chunk_size))
all_files <- c()

for (chunk in draw_chunks) {
  chunk_file <- paste0("analysis/data-derived/tmp_psa_chunk_", min(chunk), "_", max(chunk), ".rds")

  if (file.exists(chunk_file)) {
    message("⏩ Skipping chunk ", min(chunk), "-", max(chunk), " (already computed)")
    all_files <- c(all_files, chunk_file)
    next
  }

  message("▶ Running PSA for draws: ", min(chunk), "-", max(chunk))
  grid_chunk <- scenario_grid_psa %>% filter(draw %in% chunk)

  chunk_results <- future_pmap_dfr(
    grid_chunk,
    function(draw, vaccine_uptake, vsly_factor, cost_factor) {
      psa_vals <- psa_params %>% filter(draw == !!draw)
      infection_rate <- psa_vals$infection_rate
      cost_vaccine_total <- psa_vals$cost_vaccine_total
      los <- psa_vals$los

      ve_hosp <- params_fixed$ifr_ihrs$ve_h
      ve_death <- params_fixed$ifr_ihrs$ve_d

      map_dfr(policy_list, function(pol) {
        uk_base <- create_country_data(iso3c = "GBR",
                                       ifr = params_fixed$ifr_ihrs$ifr_naive,
                                       ihr = params_fixed$ifr_ihrs$ihr_naive) %>%
          left_join(employment_tbl, by = "age_group") %>%
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
        ) %>% mutate(vsly = vsly * vsly_factor)

        params_draw <- params_fixed
        params_draw$los <- los

        loss <- calculate_losses(df_burden, params_draw)
        vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake,
                                           cost_vaccine_total * cost_factor)

        tibble(
          draw = draw,
          policy_name = pol$name,
          vaccine_uptake,
          vsly_factor,
          cost_factor,
          nmb = loss$total_loss - vax_cost
        )
      })
    }
  )

  saveRDS(chunk_results, chunk_file)
  all_files <- c(all_files, chunk_file)
  rm(chunk_results); gc()
}

# --- Combine all chunk files ---
results_psa <- map_dfr(all_files, readRDS)
saveRDS(results_psa, "analysis/data-derived/scenario_results_psa.rds")
write_csv(results_psa, "analysis/tables/scenario_results_psa.csv")
cat("✅ PSA complete! Combined results saved to analysis/data-derived/scenario_results_psa.rds\n")
