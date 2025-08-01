#from Squire
vsl_fn <- function(iso = "GBR", vsl_path = "analysis/data-derived/vsly.rds") {

  # read in and select the data we want
  # simplified this here and just called life expectancy as le
  vsly_df <- readRDS(vsl_path) %>%
    filter(iso3c == iso) %>%
    select(age_group,
           n = Ng,
           le = lg,
           vsl,
           vsly,
           iso3c)

  return(vsly_df)

}
