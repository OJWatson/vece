# Example script to show how to plot and save tables

# THis loads our package, so that any functions we have in the R directory
# are available to us
devtools::load_all()

# This loads tidyverse which is a very helpful set of tools for data handling

# Start by reading in our data
vsly <- readRDS("analysis/data-derived/vsly.rds")

# start by let's making  a plot of life expectancy of UK age groups
le_plot <- vsly %>%
  filter(iso3c == "GBR") %>%
  ggplot(aes(age_group, lg)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Age Group") +
  ylab("Life Expectancy")

# the save_figs function is a function in the R directory that helps
# saving figures easily
save_figs(name = "GBR_life_expectancy", le_plot, width = 7, height = 5)

# and let's also show how to save a table
le_table <- vsly %>%
  filter(iso3c == "GBR") %>%
  select(age_group, lg) %>%
  rename(life_expetancy = lg)

write.csv(le_table, "analysis/tables/GBR_life_expectancy.csv", row.names = FALSE)
