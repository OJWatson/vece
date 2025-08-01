# --- Step 4: Run scenario loop ---
scenario_results <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate, efficacy_symptomatic) {

    # Calculate burden (age-stratified)
    df_burden <- calculate_disease_burden(
      df = uk_base,
      infection_rate = infection_rate,
      vaccine_uptake = vaccine_uptake,
      efficacy_symptomatic = efficacy_symptomatic
    )

    # Calculate losses
    loss <- calculate_losses(df_burden, params)

    # Vaccine cost
    vax_cost <- calculate_vaccine_cost(
      population = df_burden$n,
      uptake = vaccine_uptake,
      cost_vaccine_total = params$cost_vaccine_total
    )

    # Medical hospital burden (also included in total_loss, shown separately here)
    hospital_burden <- calculate_hospital_burden(
      hospitalisations = df_burden$hospitalisations,
      los = params$los,
      cost_per_day = params$cost_per_day
    )

    # Output result
    tibble(
      vaccine_uptake = vaccine_uptake,
      infection_rate = infection_rate,
      efficacy_symptomatic = efficacy_symptomatic,
      hospital_cost = hospital_burden,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 5: Save results ---
write_csv(scenario_results, "analysis/tables/scenario_results.csv")
saveRDS(scenario_results, "analysis/data-derived/scenario_results.rds")

# --- Optional: QA Print ---
print(scenario_results)
