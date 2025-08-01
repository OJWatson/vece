# R/loss_strategy.R

library(tidyverse)

# Mortality Loss: Deaths × Remaining Life Expectancy × VSLY
calculate_mortality_loss <- function(deaths, life_expectancy, vsly) {
  return(sum(deaths * life_expectancy * vsly, na.rm = TRUE))
}

# Productivity Loss from Hospitalisation = Hospitalisations × Days × Wage
calculate_productivity_loss_hosp <- function(hospitalisations, los, post_hosp_days, daily_wage) {
  total_days_lost <- los + post_hosp_days
  wage_safe <- ifelse(is.na(daily_wage), 0, daily_wage)
  return(sum(hospitalisations * total_days_lost * wage_safe, na.rm = TRUE))
}

# Symptomatic Infection Loss = Infections × Days × Wage
calculate_symptomatic_loss <- function(symptomatic_infections, symptomatic_days_lost, daily_wage) {
  return(sum(symptomatic_infections * symptomatic_days_lost * daily_wage, na.rm = TRUE))
}

# Long COVID Loss = Infections × Long COVID Prob × Days × Wage
calculate_long_covid_loss <- function(infections, long_covid_prob, long_covid_days, daily_wage) {
  return(sum(infections * long_covid_prob * long_covid_days * daily_wage, na.rm = TRUE))
}

# Long COVID Loss = Infections × Long COVID Prob × Days × Wage
calculate_long_covid_severity_loss <- function(hospital_unvacc,
                                               symptomatic_unvacc,
                                               hospital_vacc,
                                               symptomatic_vacc,
                                               long_covid_prob,
                                               long_covid_short_days,
                                               long_covid_medium_days,
                                               long_covid_long_days,
                                               daily_wage) {
  return(
    sum(
      c(hospital_unvacc * long_covid_prob * long_covid_long_days * daily_wage,
        symptomatic_unvacc * long_covid_prob * long_covid_medium_days * daily_wage,
        hospital_vacc * long_covid_prob * long_covid_medium_days * daily_wage,
        symptomatic_vacc * long_covid_prob * long_covid_short_days * daily_wage
      ), na.rm = TRUE)
    )
}

# Informal Caregiving Loss = Infections (age <16) × Avg Days × Daily Wage
calculate_informal_care_loss <- function(pediatric_symptomatic, care_days, daily_wage) {
  return(sum(pediatric_symptomatic * care_days * daily_wage/2, na.rm = TRUE))
}

# Hospital Burden = Hospitalisations × Avg Length of Stay × Cost per Day
calculate_hospital_burden <- function(hospitalisations, los, cost_per_day) {
  return(sum(hospitalisations * los * cost_per_day, na.rm = TRUE))
}

# Wrapper to calculate all losses and return a named tibble
calculate_losses <- function(df, params) {
  with(params, {
    mortality_loss <- calculate_mortality_loss(df$deaths, df$life_years_remaining, df$vsly)
    hosp_prod_loss <- calculate_productivity_loss_hosp(df$hospitalisations, los, post_hosp_days, df$daily_wage)
    symptomatic_loss <- calculate_symptomatic_loss(df$symptomatic_infections, symptomatic_days_lost, df$daily_wage)
    # this is a new function based on having different long covid durations
    long_covid_loss <- calculate_long_covid_severity_loss(
      hospital_unvacc = df$hospitalisations_unvacc - df$deaths_unvacc,
      symptomatic_unvacc = df$symptomatic_unvacc_infections - df$hospitalisations_unvacc,
      hospital_vacc = df$hospitalisations_vacc - df$deaths_vacc,
      symptomatic_vacc = df$symptomatic_vacc_infections - df$hospitalisations_vacc,
      long_covid_prob = long_covid_prob,
      long_covid_short_days = long_covid_short_days,
      long_covid_medium_days = long_covid_medium_days,
      long_covid_long_days = long_covid_long_days,
      daily_wage = df$daily_wage)
    # i used the mean wage across all age groups for the care loss
    informal_care_loss <- calculate_informal_care_loss(df$pediatric_symptomatic, care_days, mean(df$daily_wage))
    hospital_medical_cost <- calculate_hospital_burden(df$hospitalisations, los, cost_per_day)

    return(tibble(
      mortality_loss = mortality_loss,
      hosp_prod_loss = hosp_prod_loss,
      hospital_medical_cost = hospital_medical_cost,
      symptomatic_loss = symptomatic_loss,
      long_covid_loss = long_covid_loss,
      informal_care_loss = informal_care_loss,
      total_loss = sum(mortality_loss, hosp_prod_loss, hospital_medical_cost, symptomatic_loss, long_covid_loss, informal_care_loss, na.rm = TRUE)
    ))
  })
}
