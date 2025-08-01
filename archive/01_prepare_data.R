rm(list = ls())  # Clear all objects in the R environment
library(tidyverse)  # Load the tidyverse collection (includes dplyr, ggplot2, readr, etc.)
library(squire)  # Load squire for SEIR model parameters and population data
# --------------------------------------------
# Load and prepare population data and parameters from Squire
# --------------------------------------------
uk_pop_df <- get_population(iso3c = "GBR")  # Load UK population by 5-year age group
squire_params <- parameters_explicit_SEEIR(country = "United Kingdom")  # Load parameters like IFR and hospitalisation rates
uk_pop_df$prob_hosp <- squire_params$prob_hosp  # Add hospitalisation probability to data frame
uk_pop_df$ifr <- squire_params$ifr  # Add IFR (Infection Fatality Rate) to data frame
# --------------------------------------------
# Load external economic and demographic data
# --------------------------------------------
vsly_df <- readRDS("C:/Users/Hp_User/Downloads/vsly (2).rds")  # Load VSLY data
vsl_df <- readRDS("C:/Users/Hp_User/Downloads/vsl (1).rds")  # Load VSL data
life_expectancy <- read_csv("C:/Users/Hp_User/OneDrive/Documents/dissertation R/life_expectancy.csv")  # Load life expectancy
# --------------------------------------------
# Filter and clean external data for UK only
# --------------------------------------------
vsly_uk <- vsly_df %>% filter(iso3c == "GBR")  # Subset VSLY for UK
vsl_uk_value <- vsl_df %>% filter(iso3c == "GBR") %>% pull(vsl)  # Pull numeric UK VSL value
life_expectancy_uk <- life_expectancy %>% filter(iso3c == "GBR")  # Subset life expectancy for UK
# --------------------------------------------
# Harmonise age groups across datasets
# --------------------------------------------
fix_age_groups <- function(df) {
  df %>%
    mutate(age_group = recode(age_group,
                              "0-5" = "0-4", "5-10" = "5-9", "10-15" = "10-14", "15-20" = "15-19",
                              "20-25" = "20-24", "25-30" = "25-29", "30-35" = "30-34", "35-40" = "35-39",
                              "40-45" = "40-44", "45-50" = "45-49", "50-55" = "50-54", "55-60" = "55-59",
                              "60-65" = "60-64", "65-70" = "65-69", "70-75" = "70-74", "75-80" = "75-79"
    ))
}

vsly_uk <- fix_age_groups(vsly_uk)  # Fix age groups in VSLY
life_expectancy_uk <- fix_age_groups(life_expectancy_uk)  # Fix age groups in life expectancy
life_expectancy_uk <- life_expectancy_uk %>% rename(life_years_remaining = lg)  # Rename column
# -----------------------------
# Merge all relevant data into uk_pop_df
# -----------------------------
uk_pop_df <- get_population(iso3c = "GBR") %>%
  left_join(vsly_uk %>% select(age_group, vsly), by = "age_group") %>%
  left_join(life_expectancy_uk %>% select(age_group, life_years_remaining), by = "age_group") %>%
  mutate(
    vsl = as.numeric(vsl_uk_value),
    life_years_remaining = case_when(
      age_group == "5-9" ~ 78,
      age_group == "10-14" ~ 73,
      TRUE ~ life_years_remaining
    ),
    prob_hosp = squire_params$prob_hosp[1:17]
  )

# Infection Fatality Ratio (IFR) values adapted from Verity et al. (2020)
# Source: Verity R et al. (2020), Lancet Infect Dis, doi:10.1016/S1473-3099(20)30243-7
# Used in the Squire model: squire::parameters_explicit_SEEIR("GBR")$ifr
ifr_percent <- c(0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.06, 0.10,
                 0.16, 0.24, 0.38, 0.60, 0.94, 1.47, 2.31, 3.61, 8.82)  # age-specific IFRs in %
ifr_vector <- ifr_percent / 100  # Convert to proportions
stopifnot(length(ifr_vector) == nrow(uk_pop_df))  # Sanity check
uk_pop_df$ifr <- ifr_vector  # Safe manual assignment

glimpse(uk_pop_df)
# --------------------------------------------
# Function to calculate cost-benefits outcomes
# --------------------------------------------
calculate_covid_outputs <- function(df,
                                    infection_rate = 0.015,
                                    vaccine_uptake = 1.0,
                                    efficacy_symptomatic = 0.60,
                                    p_symptomatic = 0.678,
                                    workdays_lost_symptomatic = 1.5,
                                    hospital_days = 7,
                                    long_covid_days = 28,
                                    long_covid_rate = 0.04,
                                    daily_wage = 128,
                                    hospital_cost_per_day = 933,
                                    cost_vaccine = 10,
                                    cost_delivery = 5,
                                    avg_informal_days = 3,
                                    informal_care_wage = 128) {

  df <- df %>%
    mutate(
      total_infections = n * infection_rate,  # Estimate total infections
      symptomatic_infections = total_infections * p_symptomatic,  # Estimate symptomatic cases
      averted_symptomatic = symptomatic_infections * vaccine_uptake * efficacy_symptomatic,  # Averted cases due to vaccine
      averted_hosp_cases = total_infections * prob_hosp * vaccine_uptake * efficacy_symptomatic,  # Averted hospitalisations
      averted_long_covid_cases = total_infections * long_covid_rate * vaccine_uptake * efficacy_symptomatic,  # Averted long COVID
      averted_deaths = total_infections * ifr * vaccine_uptake * efficacy_symptomatic,  # Averted deaths
      # Cost-benefit outputs
      mortality_loss = averted_deaths * life_years_remaining * vsly,  # Monetised death prevention using VSLY
      hospital_burden = total_infections * prob_hosp * hospital_days * hospital_cost_per_day,  # Hospitalisation cost
      hospital_productivity_loss = total_infections * prob_hosp * hospital_days * daily_wage,  # Productivity loss from hospital
      symptomatic_loss = symptomatic_infections * workdays_lost_symptomatic * daily_wage,  # Productivity loss from symptoms
      long_covid_burden = total_infections * long_covid_rate * long_covid_days * daily_wage,  # Productivity loss from long COVID
      vaccination_cost = (cost_vaccine + cost_delivery) * n * vaccine_uptake,  # Total cost of vaccination
      # Informal caregiving (children < 15 years)
      informal_care_burden = if_else(age_group %in% c("0-4", "5-9", "10-14"),
                                     total_infections * avg_informal_days * informal_care_wage, 0)
    )

  return(df)
}

# --------------------------------------------
# Apply the function and summarise outcomes
# --------------------------------------------
results_df <- calculate_covid_outputs(uk_pop_df)  # Run calculations on population data

# Aggregate summary for total outputs across age groups
summary_table <- results_df %>%
  summarise(
    Mortality_Loss = sum(mortality_loss, na.rm = TRUE),
    Hospital_Burden = sum(hospital_burden, na.rm = TRUE),
    Hospital_Productivity_Loss = sum(hospital_productivity_loss, na.rm = TRUE),
    Symptomatic_Loss = sum(symptomatic_loss, na.rm = TRUE),
    Long_COVID_Burden = sum(long_covid_burden, na.rm = TRUE),
    Informal_Caregiving_Burden = sum(informal_care_burden, na.rm = TRUE),
    Vaccination_Cost = sum(vaccination_cost, na.rm = TRUE)
  )

print(summary_table)  # View total cost-consequence outputs
saveRDS(uk_pop_df, "data-derived/uk_pop_df.rds")
