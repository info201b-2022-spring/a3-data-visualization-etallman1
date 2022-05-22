library(dplyr)

inc_trends <- read.csv("incarceration_trends.csv")

no_na <- na.omit(inc_trends)

region_summary <- inc_trends %>%
  group_by(region) %>%
  summarise()

state_reg <- select(inc_trends, state, region) %>%
  group_by(state) %>%
  summarise(region) %>%
  distinct() 

region_distribution_no_na <- no_na %>%
  group_by(region) %>%
  summarise(occurrences = n())

original_region_distribution <- inc_trends %>%
  group_by(region) %>%
  summarise(occurrences = n())

year_summary <- inc_trends %>%
  group_by(year) %>%
  summarise(occurrences = n())


inc_trends$na_count <- rowSums(
  is.na(inc_trends)
)

na_by_year <- aggregate(
  inc_trends$na_count,
  by = list(year = inc_trends$year),
  FUN = sum
)

inc_2016 <- inc_trends %>%
  filter(year == 2016)

inc_2006 <- inc_trends %>%
  filter(year == 2006)

inc_2011 <- inc_trends %>%
  filter(year == 2011)

race_2006_state_pop_rate <- inc_2006 %>%
  select(state, total_prison_pop_rate,
    aapi_prison_pop_rate,
         black_prison_pop_rate,latinx_prison_pop_rate,
         native_prison_pop_rate,white_prison_pop_rate,)
race_2006_state_pop_rate <- aggregate(
  . ~ state, race_2006_state_pop_rate, sum
) 
race_2006_state_pop_rate <- merge(
  race_2006_state_pop_rate, state_reg, all.x = T
)

race_2011_state_pop_rate <- inc_2011 %>%
  select(state, total_prison_pop_rate,
         aapi_prison_pop_rate,
         black_prison_pop_rate,latinx_prison_pop_rate,
         native_prison_pop_rate,white_prison_pop_rate,)
race_2011_state_pop_rate <- aggregate(
  . ~ state, race_2011_state_pop_rate, sum
) 
race_2011_state_pop_rate <- merge(
  race_2011_state_pop_rate, state_reg, all.x = T
)

race_2016_state_pop_rate <- inc_2016 %>%
  select(state, total_prison_pop_rate,
         aapi_prison_pop_rate,
         black_prison_pop_rate,latinx_prison_pop_rate,
         native_prison_pop_rate,white_prison_pop_rate,)
race_2016_state_pop_rate <- aggregate(
  . ~ state, race_2016_state_pop_rate, sum
) 
race_2016_state_pop_rate <- merge(
  race_2016_state_pop_rate, state_reg, all.x = T
)

race_2006_REGION_pop_rate <- race_2006_state_pop_rate %>%
  subset(select = -c(state)) 
race_2006_REGION_pop_rate <- aggregate(
  . ~ region, race_2006_REGION_pop_rate, sum
  ) %>%
  mutate(non_white_pop_rate_total = aapi_prison_pop_rate + black_prison_pop_rate + 
           latinx_prison_pop_rate + native_prison_pop_rate) %>%
  mutate(non_white_pop_rate_DIFF = 
    non_white_pop_rate_total - total_prison_pop_rate) %>%
  mutate(white_pop_rate_DIFF = 
    white_prison_pop_rate - total_prison_pop_rate) %>%
  mutate_if(is.numeric, round)

  
#ratio version
  #mutate_if(is.numeric, round) %>%
  #mutate(non_white_pop_rate_RATIO = round(
   # non_white_pop_rate_total / total_prison_pop_rate, digits = 2)) %>%
  #mutate(white_pop_rate_RATIO = round(
   # white_prison_pop_rate / total_prison_pop_rate, digits = 2)) 

race_2011_REGION_pop_rate <- race_2011_state_pop_rate %>%
  subset(select = -c(state)) 
race_2011_REGION_pop_rate <- aggregate(
  . ~ region, race_2011_REGION_pop_rate, sum
) %>%
  mutate(non_white_pop_rate_total = aapi_prison_pop_rate + black_prison_pop_rate + 
           latinx_prison_pop_rate + native_prison_pop_rate) %>%
  mutate(non_white_pop_rate_DIFF = 
           non_white_pop_rate_total - total_prison_pop_rate) %>%
  mutate(white_pop_rate_DIFF = 
           white_prison_pop_rate - total_prison_pop_rate) %>%
  mutate_if(is.numeric, round)

race_2016_REGION_pop_rate <- race_2016_state_pop_rate %>%
  subset(select = -c(state)) 
race_2016_REGION_pop_rate <- aggregate(
  . ~ region, race_2016_REGION_pop_rate, sum
) %>%
  mutate(non_white_pop_rate_total = aapi_prison_pop_rate + black_prison_pop_rate + 
           latinx_prison_pop_rate + native_prison_pop_rate) %>%
  mutate(non_white_pop_rate_DIFF = 
           non_white_pop_rate_total - total_prison_pop_rate) %>%
  mutate(white_pop_rate_DIFF = 
           white_prison_pop_rate - total_prison_pop_rate) %>%
  mutate_if(is.numeric, round)

region_pop_rate_data <- list()
region_pop_rate_data$rates_2006 <- race_2006_REGION_pop_rate
region_pop_rate_data$rates_2011 <- race_2011_REGION_pop_rate
region_pop_rate_data$rates_2016 <- race_2016_REGION_pop_rate

a2006_df <- race_2006_REGION_pop_rate %>%
  mutate(year = 2006) %>%
  mutate(region_n_year = paste(a2006_df$region, a2006_df$year))

a2011_df <- race_2011_REGION_pop_rate %>%
  mutate(year = 2011)%>%
  mutate(region_n_year = paste(a2011_df$region, a2011_df$year))

a2016_df <- race_2016_REGION_pop_rate %>%
  mutate(year = 2016)%>%
  mutate(region_n_year = paste(a2016_df$region, a2016_df$year))

combined_dfs <- rbind(a2006_df, a2011_df) %>%
  distinct() %>%
  rbind(combined_dfs, a2016_df) %>%
  distinct()

#combined_dfs <- mutate(race_2006_REGION_pop_rate, year = 2006) %>%
 # rbind(combined_dfs, mutate(race_2011_REGION_pop_rate, year = 2011)) %>%
  #distinct() %>%
#  rbind(combined_dfs, mutate(race_2016_REGION_pop_rate, year = 2016)) %>%
#  distinct() %>%
#  mutate(region_n_year = paste(combined_dfs$region, combined_dfs$year))

library(rworldmap) 
library(RColorBrewer)










