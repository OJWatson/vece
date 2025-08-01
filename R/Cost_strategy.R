# R/cost_strategy.R

library(tidyverse)

# Function to calculate cost of vaccination programme
calculate_vaccine_cost <- function(population, uptake, cost_vaccine_total) {
  return(sum(cost_vaccine_total * population * uptake, na.rm = TRUE))
}
