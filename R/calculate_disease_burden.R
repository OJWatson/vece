# R/calculate_disease_burden.R

library(tidyverse)

calculate_disease_burden <- function(df, infection_rate, vaccine_uptake, ve_hosp, ve_death, p_symptomatic = 0.67, long_covid_rate = 0.133) {
  df %>%
    mutate(
      total_infections = n * infection_rate,
      # calculate these for each vaccine group
      symptomatic_infections = total_infections * p_symptomatic,
      symptomatic_vacc_infections = total_infections * p_symptomatic * vaccine_uptake,
      symptomatic_unvacc_infections = total_infections * p_symptomatic * (1-vaccine_uptake),
      hospitalisations_vacc = symptomatic_infections * ihr * vaccine_uptake * (1-ve_hosp),
      hospitalisations_unvacc = symptomatic_infections * ihr * (1-vaccine_uptake),
      # here for deaths we have to multiply by (ifr/(p_symptomatic*ihr)) which gives us the hfr
      # and then also by (1 - ((1-ve_death)/(1-ve_hosp))) which is the efficacy against breakthrough hospitalisations
      deaths_vacc = hospitalisations_vacc * (ifr/(p_symptomatic*ihr)) * (1 - ((1-ve_death)/(1-ve_hosp))),
      deaths_unvacc = hospitalisations_unvacc * (ifr/(p_symptomatic*ihr)),
      pediatric_symptomatic = if_else(age_group %in% c("0-5", "5-10"), symptomatic_infections, 0)
    ) %>%
    mutate(deaths = deaths_vacc + deaths_unvacc,
           hospitalisations = hospitalisations_vacc + hospitalisations_unvacc,
           ) %>%
    # Explicitly retain all original columns
    select(
      # here is where i then change name of le to life_year_remaining
      age_group, n, ifr, ihr, vsly, life_years_remaining = le, daily_wage, vaccine_uptake,
      total_infections,
      symptomatic_infections, symptomatic_vacc_infections, symptomatic_unvacc_infections,
      hospitalisations, hospitalisations_vacc, hospitalisations_unvacc,
      deaths, deaths_vacc, deaths_unvacc,
      pediatric_symptomatic
    )
}
