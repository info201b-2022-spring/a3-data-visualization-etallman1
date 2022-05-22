library(dplyr)
library(tidyr)
library(rworldmap) 
library(RColorBrewer)
library(ggplot2)
library(stringr)

inc_trends <- read.csv("incarceration_trends.csv")

#Checking which cols have missing data/when-------------------------

na_count <- function(the_year){
  total <- inc_trends %>%
    filter(year == the_year) %>%
    sapply(function(y) sum(length(which(is.na(y))))) %>%
    data.frame() %>%
    View()
}

#na_count(1998)

#Handy info----------------------------------------------------------

region_summary <- inc_trends %>%
  group_by(region) %>%
  summarise()

state_reg <- select(inc_trends, state, region) %>%
  group_by(state) %>%
  summarise(region) %>%
  distinct() 

#Cleaning--------------------------------------------------------------


filtered_1998_to_2018 <- inc_trends %>%
  filter(year == 1998 | year == 1999 | year == 2000 | 
           year == 2001 | year == 2002 | year == 2003 | year == 2004 | year == 2005 |
           year == 2006 | year == 2007 | year == 2008 | year == 2009  | year == 2010 | 
           year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | 
           year == 2016 | year == 2017 | year == 2018) %>%
  select(year, state, county_name, region, urbanicity, division, total_jail_pop_rate,
         aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, 
         native_jail_pop_rate, white_jail_pop_rate
  ) 
  
year_avg <- filtered_1998_to_2018 %>%
  group_by(year) %>%
summarise(total_mean = mean(total_jail_pop_rate, na.rm = TRUE), 
          aapi_mean = mean(aapi_jail_pop_rate, na.rm = TRUE), 
            black_mean = mean(black_jail_pop_rate, na.rm = TRUE), 
          latino_mean = mean(latinx_jail_pop_rate, na.rm = TRUE), 
            native_mean = mean(native_jail_pop_rate, na.rm = TRUE), 
          white_mean = mean(white_jail_pop_rate, na.rm = TRUE))

year_avg_simplified <- year_avg %>%
  select(-white_mean, -year, -total_mean)  
year_avg_simplified <- mutate(year_avg_simplified, non_white_mean_total = 
                                rowSums(year_avg_simplified)) %>%
  select(non_white_mean_total) 
year_avg_simplified <-  cbind(year_avg, year_avg_simplified) %>%
  select(year, total_mean, white_mean, non_white_mean_total) %>%
  rename(total_population = total_mean, white = white_mean, non_white = non_white_mean_total) %>%
  gather(
    key = "race",
    value = "mean_rate",
    -year
  ) 
  

state_avg <- filtered_1998_to_2018 %>%
  group_by(state) %>%
  summarise(total_mean = mean(total_jail_pop_rate, na.rm = TRUE), 
            aapi_mean = mean(aapi_jail_pop_rate, na.rm = TRUE), 
            black_mean = mean(black_jail_pop_rate, na.rm = TRUE), 
            latino_mean = mean(latinx_jail_pop_rate, na.rm = TRUE), 
            native_mean = mean(native_jail_pop_rate, na.rm = TRUE), 
            white_mean = mean(white_jail_pop_rate, na.rm = TRUE))
state_avg_simplified <- state_avg %>%
  select(-white_mean, -state, -total_mean)  
state_avg_simplified <- mutate(state_avg_simplified, non_white_mean_total = 
                                  rowSums(state_avg_simplified)) %>%
  select(non_white_mean_total) 
state_avg_simplified <-  cbind(state_avg, state_avg_simplified) %>%
  select(state, non_white_mean_total) %>%
  na.omit()


region_avg <- filtered_1998_to_2018 %>%
  group_by(region) %>%
  summarise(total_mean = mean(total_jail_pop_rate, na.rm = TRUE), 
            aapi_mean = mean(aapi_jail_pop_rate, na.rm = TRUE), 
            black_mean = mean(black_jail_pop_rate, na.rm = TRUE), 
            latino_mean = mean(latinx_jail_pop_rate, na.rm = TRUE), 
            native_mean = mean(native_jail_pop_rate, na.rm = TRUE), 
            white_mean = mean(white_jail_pop_rate, na.rm = TRUE))

region_avg_simplified <- region_avg %>%
  select(-white_mean, -region, -total_mean)  
region_avg_simplified <- mutate(region_avg_simplified, non_white_mean_total = 
                                rowSums(region_avg_simplified)) %>%
  select(non_white_mean_total) 
region_avg_simplified <-  cbind(region_avg, region_avg_simplified) %>%
  select(region, total_mean, white_mean, non_white_mean_total) %>%
  rename(total_population = total_mean, white = white_mean, non_white = non_white_mean_total) %>%
  gather(
    key = "race",
    value = "mean_rate",
    -region
  ) 

region_2018_avg <- filtered_1998_to_2018 %>%
  filter(year == 2018) %>%
  group_by(region) %>%
  summarise(total_mean = mean(total_jail_pop_rate, na.rm = TRUE), 
            aapi_mean = mean(aapi_jail_pop_rate, na.rm = TRUE), 
            black_mean = mean(black_jail_pop_rate, na.rm = TRUE), 
            latino_mean = mean(latinx_jail_pop_rate, na.rm = TRUE), 
            native_mean = mean(native_jail_pop_rate, na.rm = TRUE), 
            white_mean = mean(white_jail_pop_rate, na.rm = TRUE))

region_2018_avg_simplified <- region_2018_avg %>%
select(-white_mean, -region, -total_mean) 
region_2018_avg_simplified <- mutate(region_2018_avg_simplified, non_white_mean_total = 
                                  rowSums(region_2018_avg_simplified)) %>%
  select(non_white_mean_total) 
region_2018_avg_simplified <-  cbind(region_2018_avg, region_2018_avg_simplified) %>%
  select(region, total_mean, white_mean, non_white_mean_total) %>%
  rename(total_population = total_mean, white = white_mean, non_white = non_white_mean_total) %>%
  gather(
    key = "race",
    value = "mean_rate",
    -region
  ) 

race_only_region_2018 <- slice(region_2018_avg_simplified, 5:12)



avg_sum_year <- combined_dfs %>%    
  group_by(year) %>%
  summarise(white = mean(white_prison_pop_rate), non_white = mean(non_white_pop_rate_total))%>%
  round()


gathered <- avg_sum_year %>%
  gather(
    key = "race",
    value = "population_rate",
    -year
  )

gathered2 <- combined_dfs %>%
  group_by(region) %>%
  summarise(white = mean(white_prison_pop_rate), non_white = mean(non_white_pop_rate_total), all_population = mean(total_prison_pop_rate)) %>%
  gather(
    key = "race",
    value = "population_rate",
    -region
  )




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











