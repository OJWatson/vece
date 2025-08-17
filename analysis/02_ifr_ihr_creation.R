library(ggplot2)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

## assumptions
# Assumptions:
#
# IHR of 6 to 17 is same as 3 to 17 to match IFR band
# IHR and IFR of 0-4 is same as above
# Ratio of CIs to central estimate is preserved
#
# Uptake in 0-14 is 0
# Uptake is scaled to match number of vaccines administered within 6 months of UKHSA study midpoint
#
# VE of younger age groups same as 65_69 for death, but same as 70_74 for hosp since its higher
#
# log linear trend to infer top two age groups weighting split

## UK Population

pop <- c(3572007,3925921,4150287,4011468,
         4097542,4427747,4700198,4636593,
         4446226,4043242,4522878,4625265,
         4181669, 3489709,3120459,2843315,
         1763312,1095652,611719)
#taken from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland

### UKHSA SEVERITY

df1 <- read.csv("analysis/data-raw/seasonal_covid_vax/ukhsa_ihr_ifr.csv")

ggplot(df1, aes(x = age, y = ihr)) +
  geom_point() +
  geom_errorbar(aes(ymin = ihr_lb, ymax = ihr_ub), width = 0.2) +
  scale_y_log10() +
  theme_minimal()

ggplot(df1, aes(x = age, y = ifr)) +
  geom_point() +
  geom_errorbar(aes(ymin = ifr_lb, ymax = ifr_ub), width = 0.2) +
  scale_y_log10() +
  theme_minimal()

df1 <- df1 %>%
  bind_rows(df1 %>% filter(age == "3_17") %>% mutate(age = "0_4")) %>%
  bind_rows(df1 %>% filter(age == "75_+") %>% mutate(age = "80_+")) %>%
  mutate(age = if_else(age == "75_+","75_79",age))
df1 <- df1 %>%
  mutate(
    age_lower = as.numeric(sub("_.*", "", age)),
    age_upper = if_else(age == "80_+", 85, suppressWarnings(as.numeric(sub(".*_", "", age))) + 1),
    age_mid = (age_lower + age_upper) / 2
  )

new_age_midpoints <- seq(7.5, 72.5, by = 5)
interpolated_tbl <- map_dfc(df1 %>% dplyr::select(-starts_with("age")) %>% names(),
                            ~ {
                              exp(approx(x = df1$age_mid, y = log(df1[[.x]]), xout = new_age_midpoints)$y) %>% as_tibble_col(column_name = .x)
                            })

interpolated_tbl <- map_dfc(df1 %>% dplyr::select(-starts_with("age")) %>% names(),
                            ~ {
                              spline_fn <- splinefun(x = df1$age_mid, y = log(df1[[.x]]), method = "natural")
                              interpolated_vals <- exp(spline_fn(new_age_midpoints))
                              as_tibble_col(interpolated_vals, column_name = .x)
                            })

df1 <- df1 %>%
  bind_rows(
    tibble(age = c("5_9", "10_14", "15_19",
                   "20_24", "25_29", "30_34", "35_39",
                   "40_44", "45_49", "50_54", "55_59",
                   "60_64", "65_69", "70_74"),
           interpolated_tbl)
  ) %>%
  filter(! age %in% c("3_17",  "18_34", "35_44", "45_54", "55_64" ,"65_74", "75-79")) %>%
  dplyr::select(-starts_with("age_")) %>%
  mutate(age = factor(age, levels = c(paste(seq(0, 75, by = 5), seq(4, 79, by = 5), sep = "_"), "80_+")))

ggplot(df1, aes(x = age, y = ihr)) +
  geom_point() +
  geom_errorbar(aes(ymin = ihr_lb, ymax = ihr_ub), width = 0.2) +
  scale_y_log10() +
  theme_minimal()

ggplot(df1, aes(x = age, y = ifr)) +
  geom_point() +
  geom_errorbar(aes(ymin = ifr_lb, ymax = ifr_ub), width = 0.2) +
  scale_y_log10() +
  theme_minimal()

df1 <- df1 %>% #ratio of CIs to central estimate are applied directly to estimated central estimate
  mutate(ihr_lb = ihr_lb/ihr,
         ihr_ub = ihr_ub/ihr,
         ifr_lb = ifr_lb/ifr,
         ifr_ub = ifr_ub/ifr)

### ONS UPTAKE

df2 <- read.csv("analysis/data-raw/seasonal_covid_vax/fourth_dose.csv")
df2 <- df2 %>%
  rename(age = The.number.of.eligible.individuals.with.three.vaccinations.who.continue.to.their.fourth.vaccination.is.higher.in.older.age.groups.,
         uptake = X) %>%
  filter(row_number() >= 7) %>%
  mutate(age = case_when(
    age == "80+" ~ "80_+",
    TRUE ~ gsub("-", "_", age)
  )) %>%
  mutate(uptake = as.numeric(uptake) /100)

# ggplot(df2, aes(x = age, y = uptake)) +
#   geom_bar(stat = "identity") +
#   xlab("Age group label") +
#   ylab("Uptake") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

df2 <- df2 %>%
  bind_rows(
    tibble(age = c("0_4", "5_9", "10_14"), uptake = 0)
  ) %>%
  mutate(
    age_lower = as.numeric(sub("_.*", "", age)),
    age_upper = if_else(age == "80_+", 85, suppressWarnings(as.numeric(sub(".*_", "", age))) + 1),
    age_mid = (age_lower + age_upper) / 2
  )
new_age_midpoints <- seq(17.5, 77.5, by = 5)
interpolated_values <- approx(
  x = df2$age_mid,
  y = df2$uptake,
  xout = new_age_midpoints
)

df2 <- df2 %>%
  bind_rows(
    tibble(age = c("15_19",
                   "20_24", "25_29", "30_34", "35_39",
                   "40_44", "45_49", "50_54", "55_59",
                   "60_64", "65_69", "70_74", "75_79"),
           uptake = interpolated_values$y)
  ) %>%
  filter(! age %in% c("70_79" ,"60_69" ,"50_59", "40_49" ,"30_39", "18_29")) %>%
  dplyr::select(age,uptake)

#UKHSA IHR/IFR study period was 14 November 2023 and 3 March 2024, midpoint of which is 8 January 2024
#11801217 people were vaccinated in 6 months before midpoint, between 8 July 2023 and 8 Jan 2024:
# https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
aggpop <- tibble::tibble(
  age = c("0_4", "5_9", "10_14", "15_19", "20_24", "25_29",
          "30_34", "35_39", "40_44", "45_49", "50_54", "55_59",
          "60_64", "65_69", "70_74", "75_79", "80_+"),
  pop = c(pop[1:16], sum(pop[17:19]))
) %>%
  left_join(df2, by = "age")
scaling <- 11801217/sum(aggpop$pop*aggpop$uptake)

df2 <- df2 %>% mutate(uptake = scaling*uptake) %>%
  mutate(age = factor(age, levels = c(paste(seq(0, 75, by = 5), seq(4, 79, by = 5), sep = "_"), "80_+")))

ggplot(df2, aes(x = age, y = uptake)) +
  geom_bar(stat = "identity") +
  xlab("Age group label") +
  ylab("Uptake") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### KEELING VE

df3 <- read.csv("analysis/data-raw/seasonal_covid_vax/keeling_ve.csv")

ggplot(df3, aes(x = age, y = ve_h)) +
  geom_point() +
  geom_errorbar(aes(ymin = ve_h_lb, ymax = ve_h_ub), width = 0.2) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))

ggplot(df3, aes(x = age, y = ve_d)) +
  geom_point() +
  geom_errorbar(aes(ymin = ve_d_lb, ymax = ve_d_ub), width = 0.2) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))

weights <- pop[17:19] / sum(pop[17:19])
weighted_row <- df3 %>%
  filter(age %in% c("80_84", "85_89", "90_94")) %>%
  summarise(across(where(is.numeric), ~ sum(.x * weights))) %>%
  mutate(age = "80_+") %>%
  select(age, everything())
df3 <- bind_rows(df3, weighted_row)
#row_younger <- df3 %>% filter(age == "65_69")
row_younger <- df3 %>%
  filter(age %in% c("65_69", "70_74")) %>%
  summarise(
    across(
      everything(),
      ~ if (cur_column() %in% grep("^ve_d", names(df3), value = TRUE)) {
        .[age == "65_69"]
      } else {
        .[age == "70_74"]
      }
    )
  )

df3 <- data.frame(age = c(paste(seq(0, 75, by = 5), seq(4, 79, by = 5), sep = "_"), "80_+")) %>%
  left_join(df3, by = "age") %>%
  mutate(across(
    -age,
    ~ if_else(is.na(.), row_younger[[cur_column()]], .)
  )) %>%
  mutate(age = factor(age, levels = c(paste(seq(0, 75, by = 5), seq(4, 79, by = 5), sep = "_"), "80_+")))

ggplot(df3, aes(x = age, y = ve_h)) +
  geom_point() +
  geom_errorbar(aes(ymin = ve_h_lb, ymax = ve_h_ub), width = 0.2) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))

ggplot(df3, aes(x = age, y = ve_d)) +
  geom_point() +
  geom_errorbar(aes(ymin = ve_d_lb, ymax = ve_d_ub), width = 0.2) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))

### COMBINED

df <- df1 %>% left_join(df2, by = "age") %>% left_join(df3, by = "age") %>%
  mutate(ihr_naive = ihr/((1-uptake)+uptake*(1-ve_h))) %>%
  #mutate(ihr_naive_lb = ihr_lb/((1-uptake)+uptake*(1-ve_h_ub))) %>%
  #mutate(ihr_naive_ub = ihr_ub/((1-uptake)+uptake*(1-ve_h_lb))) %>%
  mutate(ihr_naive_lb = ihr_lb*ihr_naive) %>%
  mutate(ihr_naive_ub = ihr_ub*ihr_naive) %>%
  mutate(ifr_naive = ifr/((1-uptake)+uptake*(1-ve_d))) %>%
  #mutate(ifr_naive_lb = ifr_lb/((1-uptake)+uptake*(1-ve_d_ub))) %>%
  #mutate(ifr_naive_ub = ifr_ub/((1-uptake)+uptake*(1-ve_d_lb)))
  mutate(ifr_naive_lb = ifr_lb*ifr_naive) %>%
  mutate(ifr_naive_ub = ifr_ub*ifr_naive)

gg1 <-ggplot(df, aes(x = age, y = ihr_naive)) +
  geom_point() +
  geom_errorbar(aes(ymin = ihr_naive_lb, ymax = ihr_naive_ub), width = 0.2) +
  scale_y_log10() +
  theme_bw()
gg1

gg2 <- ggplot(df, aes(x = age, y = ifr_naive)) +
  geom_point() +
  geom_errorbar(aes(ymin = ifr_naive_lb, ymax = ifr_naive_ub), width = 0.2) +
  scale_y_log10() +
  theme_bw()
gg2

# last thing to correct the upp two age groups
df <- df %>% mutate(age_mid = c(77.5, 2.5, 90, 7.5, seq(12.5, 72.5, 5))) %>%
  arrange(age_mid)

# weights
w1 <- pop[16]  # 75–79
w2 <- sum(tail(pop, 3))  # 80+

# average value over both groups (current 80+ value)
bar_x <- df %>% filter(age == "80_+") %>% pull(ifr_naive)

# estimate slope from log-scale between 70–74 and 75–79+ range
x75 <- bar_x
x70 <- df %>% filter(age == "70_74") %>% pull(ifr_naive)
x50 <- df %>% filter(age == "50_54") %>% pull(ifr_naive)
log_slope <- (log(x70) - log(x50)) / 20  # per 5-year group

# solve for x1 (75–79)
x1 <- bar_x * (w1 + w2) / (w1 + w2 * exp(log_slope * 5))

# solve for x2 (80+)
x2 <- x1 * exp(log_slope * 5)

# display multiplicative factors (to apply to current identical value)
multiplier_75_79 <- x1 / x75
multiplier_80_plus <- x2 / x75

# make a new final df
df_final <- df

# and select just what we want
df_final <- df_final %>%
  select(age, starts_with("ve"), matches("naive"))
df_final$ve_d_lb[c(15,17)] <- df_final$ve_d_lb[c(14)]


# apply correction
df_final[16, 8:13] <- df_final[16, 8:13] * multiplier_75_79
df_final[17, 8:13] <- df_final[17, 8:13] * multiplier_80_plus


gg1 <-ggplot(df_final, aes(x = age, y = ihr_naive)) +
  geom_point() +
  geom_errorbar(aes(ymin = ihr_naive_lb, ymax = ihr_naive_ub), width = 0.2) +
  scale_y_log10() +
  theme_bw()
gg1
gg2 <- ggplot(df_final, aes(x = age, y = ifr_naive)) +
  geom_point() +
  geom_errorbar(aes(ymin = ifr_naive_lb, ymax = ifr_naive_ub), width = 0.2) +
  scale_y_log10() +
  theme_bw()
gg2


write.csv(df_final, file = "analysis/data-derived/ifhr_table.csv")
ggsave("analysis/plots/ihr.png", plot = gg1, height = 10, width = 10)
ggsave("analysis/plots/ifr.png", plot = gg2, height = 10, width = 10)
