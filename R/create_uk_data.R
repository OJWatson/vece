# R/create_UK_data.R
create_country_data <- function(iso3c = "GBR",
                           daily_wage = c(0, 0, 0, 18, 44, 88, 88, 110, 118, 118, 112, 112, 87, 87, 87, 87, 87),
                           ifr, ihr) {

  # --- Load population and Squire parameters ---
  # I just simplified this here a lot just using using vsl_fn
  df <- vsl_fn(iso = iso3c) %>%
    mutate(ifr = ifr,
           ihr = ihr,
           daily_wage = daily_wage)

  return(df)
}
