raw.df <- read.csv("/Users/marsikeda/Smith College/GOV 305f/Final Data project/Child Trends State Level FY2021.csv")
kg1.df <- raw.df |>
  select(state, state_abbrev, has_kingap, pct_guardexit_agelt_1, pct_guardexit_age1_to_5,pct_guardexit_age6_to_10,pct_guardexit_age11_to_15,pct_guardexit_age16_to_20)
kg2.df <- raw.df |>
  select(state, state_abbrev, has_kingap, pct_guardexit_agelt_1, pct_guardexit_age1_to_5,pct_guardexit_age6_to_10,pct_guardexit_age11_to_15,pct_guardexit_age16_to_20)

summary_kg.df <- kg1.df |>
  # ages = c(pct_guardexit_agelt_1, pct_guardexit_age1_to_5,pct_guardexit_age6_to_10,pct_guardexit_age11_to_15,pct_guardexit_age16_to_20) |>
  group_by(has_kingap) |>
  summarize(mean = mean(pct_guardexit_agelt_1, na.rm = TRUE)
            )