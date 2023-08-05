raw.df <- read.csv("/Users/marsikeda/Smith College/GOV 305f/Final Data project/Child Trends State Level FY2021.csv")
kg1.df <- raw.df |>
  select(state, state_abbrev, has_kingap, pct_guardexit_agelt_1, pct_guardexit_age1_to_5,pct_guardexit_age6_to_10,pct_guardexit_age11_to_15,pct_guardexit_age16_to_20)
kg2.df <- raw.df |>
  select(state, state_abbrev, has_kingap, pct_guardexit_agelt_1, pct_guardexit_age1_to_5,pct_guardexit_age6_to_10,pct_guardexit_age11_to_15,pct_guardexit_age16_to_20)

#summary_kg.df <- kg1.df |>
  #group_by(has_kingap) |>
  #summarize(mean = mean(pct_guardexit_agelt_1, na.rm = TRUE)
            #)

str(kg1.df$pct_guardexit_agelt_1) #checking value type in col
as.numeric(kg1.df$pct_guardexit_agelt_1) #attempt to convert to numeric produces NA because of % sign

# https://stackoverflow.com/questions/8329059/how-to-convert-character-of-percent-into-numeric-in-r

ages = c(pct_guardexit_agelt_1, pct_guardexit_age1_to_5,pct_guardexit_age6_to_10,pct_guardexit_age11_to_15,pct_guardexit_age16_to_20) |>

kg1.df <- kg1.df |>
  mutate(agelt_1_num = parse_number(pct_guardexit_agelt_1))
  
mean(kg1.df$agelt_1_num, na.rm = TRUE)

kg1.df <- kg1.df |>
  mutate(age1_to_5 = parse_number(pct_guardexit_age1_to_5),
         age6_to_10 = parse_number(pct_guardexit_age6_to_10),
         age11_to_15 = parse_number(pct_guardexit_age11_to_15),
         age16_to_20 = parse_number(pct_guardexit_age16_to_20))
  #select(agelt_1, 
         #age1_to_5, 
         #age6_to_10, 
         #age11_to_15,
         #age16_to_20)

kg1_filter.df <- kg1.df |>
  filter(has_kingap == "Yes") #filter works to filter for kingap

#Note: US row is N/A re: Kingap.

kg1a_filter.df <- kg2.df |>
  filter(has_kingap == "No")

#below makes new rows with numeric values for non-kingap states
kg1a_filter.df <- kg1a_filter.df |>
  mutate(agelt_1 = parse_number(pct_guardexit_agelt_1),
         age1_to_5 = parse_number(pct_guardexit_age1_to_5),
         age6_to_10 = parse_number(pct_guardexit_age6_to_10),
         age11_to_15 = parse_number(pct_guardexit_age11_to_15),
         age16_to_20 = parse_number(pct_guardexit_age16_to_20))

#below selects for needed columns to do math
kg1a_filter.df <- kg1a_filter.df |>
  select(state, state_abbrev,
         agelt_1, 
         age1_to_5, 
         age6_to_10, 
         age11_to_15,
         age16_to_20)

