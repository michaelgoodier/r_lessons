#lesson 2
library(tidyverse)
library(janitor)

covid_data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newAdmissions&metric=newCasesByPublishDate&metric=newDeaths28DaysByDeathDate&format=csv") %>%
  clean_names()


# pivot_longer() and pivot_wider()


areas_we_want <- c("Essex","Derbyshire","Newham")

covid_wide <- covid_data %>%
  clean_names() %>%
  filter(area_name %in% areas_we_want) %>%
  select(area_name,date,cases=new_cases_by_publish_date) %>%
  pivot_wider(names_from = area_name,values_from = cases)

#pivot longer

covid_long <- covid_wide %>%
  pivot_longer(,names_to="area",values_to="cases")

covid_long <- covid_wide %>%
  pivot_longer(2:4,names_to="area",values_to="cases")

#group_by() and summarise()

most_deaths <- covid_data %>%
  group_by(area_name) %>%
  summarise(total_deaths = sum(new_deaths28days_by_death_date,na.rm = T),
            average_deaths = mean(new_deaths28days_by_death_date,na.rm = T))

#removing Nas first
most_deaths <- covid_data %>%
  mutate(new_deaths28days_by_death_date = replace_na(new_deaths28days_by_death_date,0))

worst_day <- covid_data %>%
  group_by(area_name) %>%
  mutate(highest_deaths_area = max(new_deaths28days_by_death_date,na.rm = T)) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(highest_deaths_date = max(new_deaths28days_by_death_date,na.rm = T),
         deaths_as_a_percentage_of_highest_area = 100*(new_deaths28days_by_death_date/highest_deaths_area))

#Joins 