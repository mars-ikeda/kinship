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

# ages = c(pct_guardexit_agelt_1, pct_guardexit_age1_to_5,pct_guardexit_age6_to_10,pct_guardexit_age11_to_15,pct_guardexit_age16_to_20) |>

kg1.df <- kg1.df |>
  mutate(agelt_1_num = parse_number(pct_guardexit_agelt_1))
  
mean(kg1.df$agelt_1_num, na.rm = TRUE)

kg1.df <- kg1.df |>
  mutate(agelt_1 = parse_number(pct_guardexit_agelt_1),
         age1_to_5 = parse_number(pct_guardexit_age1_to_5),
         age6_to_10 = parse_number(pct_guardexit_age6_to_10),
         age11_to_15 = parse_number(pct_guardexit_age11_to_15),
         age16_to_20 = parse_number(pct_guardexit_age16_to_20))
  #select(agelt_1, 
         #age1_to_5, 
         #age6_to_10, 
         #age11_to_15,
         #age16_to_20)

kg_to_filter.df <- kg1.df |>
  select(state, state_abbrev, has_kingap,
          agelt_1, 
          age1_to_5, 
          age6_to_10, 
          age11_to_15,
          age16_to_20)

kg_filtered <- kg_to_filter.df |>
  group_by(has_kingap) |>
  summarize(mean_lt_1 = mean(agelt_1),
             mean1_to_5 = mean(age1_to_5),
             mean6_to_10 = mean(age6_to_10),
             mean11_to_15 = mean(age11_to_15),
             mean16_to_20 = mean(age16_to_20)
            )

#  (summarise(mean_lt_1 = mean(agelt_1),
#                      mean1_to_5 = mean(age1_to_5),
#                      mean6_to_10 = mean(age6_to_10),
#                      mean11_to_15 = mean(age11_to_15),
#                      mean16_to_20 = mean(age16_to_20)
#  )
  

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

kg1_filter.df <- kg1_filter.df |>
  select(state, state_abbrev,
         agelt_1, 
         age1_to_5, 
         age6_to_10, 
         age11_to_15,
         age16_to_20)

# to do: Make the math math.

summary_kgap <- kg1_filter.df |>
  summarise(mean_lt_1 = mean(agelt_1),
            mean1_to_5 = mean(age1_to_5),
            mean6_to_10 = mean(age6_to_10),
            mean11_to_15 = mean(age11_to_15),
            mean16_to_20 = mean(age16_to_20)
            )

summary_no_kgap <- kg1a_filter.df |>
  summarise(mean_lt_1 = mean(agelt_1),
            mean1_to_5 = mean(age1_to_5),
            mean6_to_10 = mean(age6_to_10),
            mean11_to_15 = mean(age11_to_15),
            mean16_to_20 = mean(age16_to_20)
  )
# GOV DATA IMPORT
raw_GOV.df <- read.csv("/Users/marsikeda/Smith College/GOV 305f/Final Data project/fy2021-GAP-mei.csv")

colnames(raw_GOV.df)

raw_GOV.df <- raw_GOV.df |>
  select(state,
         Effective.Date.of.Title.IV.E.GA.Plan,
         Agency.GAP...Total.Computable..TC.,
         Title.IV.E.GAP.Caseload,
         Any.GAP..Title.IV.E...Non.IV.E..Caseload,
         Title.IV.E.GAP.Participation.Rate)

Gov.df <- raw_GOV.df |>
  mutate(start = Effective.Date.of.Title.IV.E.GA.Plan,
         total = Agency.GAP...Total.Computable..TC.,
         caseload_IV_E = Title.IV.E.GAP.Caseload,
         caseload_non_IV_E = Any.GAP..Title.IV.E...Non.IV.E..Caseload,
         GAP_participation = Title.IV.E.GAP.Participation.Rate
         ) |>
  mutate(GAP_participation = parse_number(GAP_participation)) #change percentage to numeric

Gov.df <- Gov.df |>
  select(state,
         start,
         total,
         caseload_IV_E,
         caseload_non_IV_E,
         GAP_participation)

Gov.df <- Gov.df[-(43:55),] #delete rows that are blank and N/A

#need to parse date in start

Gov_parse.df <- Gov.df |>
  mutate(start = parse_date(start, "%m/%d/%Y"))

#GOV DATA EDA

ggplot(Gov_parse_num.df, (aes(x = start, y = GAP_participation,
                              size = caseload_IV_E))) +
  geom_point() +
  scale_size_area(max_size = 10)

text(Gov_parse_num.df$start, Gov_parse_num.df$GAP_participation, labels = Gov_parse_num.df$state)

text(df$x, df$y-1, labels=df$z)

typeof(Gov_parse_num.df$caseload_IV_E)

Gov_parse_num.df <- Gov_parse.df |>
  mutate(total = parse_number(total),
         caseload_IV_E = parse_number(caseload_IV_E),
         caseload_non_IV_E = parse_number(caseload_non_IV_E)
         )

str(Gov_parse.df)