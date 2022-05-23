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
year_avg_simplified$race <- str_replace_all(year_avg_simplified$race, "white", "White")
year_avg_simplified$race <- str_replace_all(year_avg_simplified$race, "non_White", "Non-White")
year_avg_simplified$race <- str_replace_all(year_avg_simplified$race, 
                                            "total_population", "Total Population")

year_avg_compared <- year_avg %>%
  select(-white_mean, -year, -total_mean)  
year_avg_compared <- mutate(year_avg_compared, non_white_mean_total = rowSums(year_avg_compared)) %>%
  select(non_white_mean_total) 
year_avg_compared <-  cbind(year_avg, year_avg_compared) %>%
  select(year, white_mean, non_white_mean_total) %>%
  mutate(diff = non_white_mean_total - white_mean) %>%
  arrange(desc(diff))
  

state_avg <- filtered_1998_to_2018 %>%
  filter(year == 2018) %>%
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

region_avg_compared_2018 <- region_2018_avg %>%
  select(-white_mean, -region, -total_mean) 
region_avg_compared_2018 <- mutate(region_avg_compared_2018, non_white_mean_total = 
                                       rowSums(region_avg_compared_2018)) %>%
  select(non_white_mean_total) 
region_avg_compared_2018 <-  cbind(region_2018_avg, region_avg_compared_2018) %>%
  mutate(diff = non_white_mean_total - white_mean) %>%
  arrange(desc(diff))

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

race_only_region_2018 <- slice(region_2018_avg_simplified, 5:12) #%>%
race_only_region_2018$race <- str_replace_all(race_only_region_2018$race, "white", "White")
race_only_region_2018$race <- str_replace_all(race_only_region_2018$race, "non_White", "Non-White")

#Values----------------------------------------------------------------------

year_highest_diff <- year_avg_compared$year[1]
mean_diff <- round(summarise(year_avg_compared, mean(diff)))
mean_diff <- format(mean_diff, big.mark = ',')
highest_diff <- round(year_avg_compared$diff[1]) 
highest_diff <- format(highest_diff, scientific  = FALSE, big.mark = ',')
region_highest_rate_diff <- region_avg_compared_2018$region[1]
state_rate_rank <- state_avg_simplified %>%
  arrange(desc(non_white_mean_total))
state_highest_rate_overall <- state_rate_rank$state[1]
highest_rate <- format(round(state_rate_rank$non_white_mean_total[1]), scientific  = FALSE, big.mark = ',')
mean_rate <- format(round(summarise(state_rate_rank, mean(non_white_mean_total))), scientific  = FALSE, big.mark = ',')
