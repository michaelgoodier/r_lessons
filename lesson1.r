############
# PACKAGES #
############

# installing packages
install.packages("tidyverse")
install.packages("janitor")
# loading packages
library(tidyverse)
library(readxl)
library(janitor)
#################
# TYPES OF DATA #
#################

# numeric integer
vibes <- 100
vibes + 50
# character
my_name <- "Michael"

# logical 
is_vibesy <- TRUE

# vector
numeric_vector_example <- c(100,30,40,50)
character_vector <- c("Michael","Is","Amazing")

# data frame / tibble
cars_data <- mtcars

############################
# READING AND WRITING DATA #
############################

# csv
covid_data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newAdmissions&metric=newCasesByPublishDate&metric=newDeaths28DaysByDeathDate&format=csv")
covid_data <- read_csv("utla_2021-02-10.csv")

# excel
infection_survey <- read_excel("covid19infectionsurveydatasets20210205.xlsx", sheet = "1a")

# exporting data
write_csv(cars_data,"cars.csv")

#####################
# MANIPULATING DATA #
#####################

# select() is for choosing columns
#remove columns with -
covid_data_clean <- select(covid_data,-newAdmissions)
#select columns by naming them
covid_data_clean <- select(covid_data,date,areaName,newDeaths28DaysByDeathDate)
#select columns by naming them and renaming them
covid_data_clean <- select(covid_data,date,area = areaName,deaths = newDeaths28DaysByDeathDate)
#select columns inclusive with :
covid_data_clean <- select(covid_data,date:areaCode)
# select columns by numbers
covid_data_clean <- select(covid_data,1:3)
# you can combine these things
covid_data_clean <- select(covid_data,1:6,-4)


#clean_names
covid_data_clean <- clean_names(covid_data)
#rename
covid_data_clean <- rename(covid_data,cases = newCasesByPublishDate,deaths=newDeaths28DaysByDeathDate)



# arrange()

#defaults to ascending
by_deaths <- arrange(covid_data,newDeaths28DaysByDeathDate)
#arrange descending with desc()
by_deaths <- arrange(covid_data,desc(newDeaths28DaysByDeathDate))

# filter()
derbyshire_by_death <- filter(by_deaths,areaName=="Derbyshire")
cases_above_300 <- filter(by_deaths,newCasesByPublishDate > 300)

derbishire_above_300 <- filter(by_deaths,areaName=="Derbyshire",newCasesByPublishDate > 300)

# > greater than
# >= greater than or equal to
# < less than
# <= less than or equal to 
# == equals
# != not equals

# & and
# | or

#hampshire no deaths but there were cases
hampshire_data <- filter(covid_data_clean,deaths==0,cases>0,areaName=="Hampshire")

# mutate() to add or change columns

with_difference <- mutate(covid_data_clean,difference=cases-deaths)

capital_letters <- mutate(covid_data_clean,areaName=toupper(areaName))

# how to use the pipe %>%

covid_data_clean <- mutate(filter(arrange(rename(select(covid_data,-newAdmissions),cases = newCasesByPublishDate,deaths=newDeaths28DaysByDeathDate),desc(deaths)),areaName=="Derbyshire"),difference=cases-deaths)

covid_data_clean <- covid_data %>%
  select(-newAdmissions) %>%
  rename(cases = newCasesByPublishDate,deaths=newDeaths28DaysByDeathDate) %>%
  arrange(desc(deaths)) %>%
  filter(areaName=="Derbyshire") %>%
  mutate(difference=cases-deaths)

# group_by() and summarise()

# pivot_longer() and pivot_wider()

#######################
# LOOPS AND FUNCTIONS #
#######################

# if / else statements

# for loops

# custom functions