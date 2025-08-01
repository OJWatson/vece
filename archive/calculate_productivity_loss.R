# Function to calculate cost-benefits outcomes
# --------------------------------------------
calculate_productivity_loss <- function(df,
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
