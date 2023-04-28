# takes the ADHS mortality data, both finalized and year-to-date;
# ensures unique records, i.e., removing duplicate values of the death certificate number;
# updates the injury date and the death date variables from character to date

# key ####
# create variables that starts with:
# d_ ; dummy variables
# calc_ ; calculated

# rené dario herrera
# 12 Jan 2023
# rherrera at coconino dot az dot gov

# Setup ####
# load packages
library(here)
library(pins)
library(tidyverse)
library(lubridate)
library(janitor)

# Pins ####
# create a pin board
mortality_folder <- board_folder("S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data")

# list the pins located on the pin board
mortality_folder %>%
  pin_list()

# check pin meta data 
mortality_folder %>%
  pin_meta("mortality-data-2010-2021")

# read historical finalized mortality data to the environment
# double check from the list, and choose the pin with the most recent data
mortality_hist <- mortality_folder %>%
  pin_read("mortality-data-2010-2021")

# inspect
glimpse(mortality_hist)

# confirm geospatial data exists n_lon & n_lat 
mortality_hist %>%
  filter(death_book_year == "2021") %>%
  select(contains("n_")) %>%
  glimpse()

# date of death is broken for 2010-2013 ####
# it has only 6 digits YYYYMM
# start
mortality_hist %>%
  filter(str_length(date_of_death) == 6) %>%
  select(c(contains("date")), contains("year"), contains("birth"), contains("dob")) %>%
  sample_n(size = 20) %>%
  glimpse()

mortality_hist %>%
  filter(str_length(date_of_death) == 8) %>%
  tabyl(death_book_year)

mortality_hist %>%
  tabyl(death_book_year)

mortality_hist %>%
  mutate(
    d_date_of_death = if_else(
      condition = str_length(date_of_death) == 6,
      true = ym(date_of_death),
      false = ymd(date_of_death)
    )
  ) %>%
  select(c(contains("date")), contains("year")) %>%
  sample_n(size = 20) %>%
  glimpse()

# end

# create new variable for date of death
mortality_hist <- mortality_hist %>%
  mutate(
    d_date_of_death = if_else(
      condition = str_length(date_of_death) == 6,
      true = ym(date_of_death),
      false = ymd(date_of_death)
    )
  )

# create new object to filter YTD data
data_years <- unique(mortality_hist$death_book_year)

# read ytd data to the environment
mortality_ytd <- mortality_folder %>%
  pin_read("mortality-data-ytd-2019-2023") %>%
  filter(!death_book_year %in% data_years) %>% # keep only most recent YTD data, because 2021 (for example) was already finalized
  mutate(d_date_of_death = ymd(date_of_death))

# inspect
glimpse(mortality_ytd)

mortality_ytd %>%
  tabyl(death_book_year) %>%
  adorn_totals()

# combine historical and year to date
mortality_data <- full_join(
  mortality_hist,
  mortality_ytd
)

# remove dummy variables
mortality_data <- mortality_data %>%
  select(-starts_with("dummy"))

# check for unique id
# 2013 data is missing death_certificate_number
mortality_data %>%
  mutate(
    cert_exists = if_else(
      condition = is.na(death_certificate_number),
      true = 0,
      false = 1
    )
  ) %>%
  tabyl(
    death_book_year, cert_exists
  )

unique(mortality_data$filename)

# create new unique id from filename & row number
mortality_data <- mortality_data %>%
  mutate(
    d_id = str_c(
      str_extract(
        string = filename,
        pattern = "(?<=S:/HIPAA Compliance/SAS Files/Coconino Deaths/All Death/).+(?=.sas7bdat)",
      ),
      "-",
      row_number()
    )
  ) 

# check that geospatial still exists
mortality_data %>%
  select(starts_with("n_")) %>%
  sample_n(size = 20) %>%
  glimpse()

# check for variable that indicates age in years 
mortality_data %>%
  select(contains("ye")) %>%
  sample_n(size = 20) %>%
  glimpse()

# tidy data ####
mortality_data <- mortality_data %>%
  mutate( # replace blank values with NA
    across(
      .cols = everything(),
      .fns = ~ na_if(x = ., y = "")
    )
  ) %>%
  ####
  # decedent_dob is broken
  ####

  mutate( # adjust relevant date fields
    d_decedent_dob = ymd(decedent_dob),
    calc_age = if_else(
      condition = is.na(decedent_years),
      true = year(d_date_of_death) - year(d_decedent_dob),
      false = as.double(decedent_years)
    ),
    d_date_of_death_year = as.character(year(d_date_of_death)),
    d_date_of_death_month = as.character(month(d_date_of_death)),
    d_date_of_death_day_month = as.character(mday(d_date_of_death)),
    d_date_of_death_day_week = wday(d_date_of_death, label = TRUE)
  )

# check
mortality_data %>%
  tabyl(death_book_year)

mortality_data %>%
  tabyl(d_date_of_death_year, death_book_year)

mortality_data %>%
  tabyl(filename)

# replace NA death book year with year from data file name
mortality_data <- mortality_data %>%
  mutate(
    death_book_year = if_else(
      condition = is.na(death_book_year),
      true = str_extract(
        string = filename,
        pattern = "[:digit:]{4}"
      ),
      false = death_book_year
    )
  ) 

# save to pin board ####

# dates, determine min and max
mortality_df_dates <- mortality_data %>%
  drop_na(death_book_year) %>%
  summarise(
    min = min(as.double(death_book_year)),
    max = max(as.double(death_book_year))
  )

# create character text string for pin board
mortality_df_name <- str_c(
  "mortality-data-combined-",
  as.character(mortality_df_dates[, 1]),
  "-",
  as.character(mortality_df_dates[, 2])
)

# create character text string for pin board
mortality_df_title <- str_c(
  "AZDHS mortality extract, combined final and year-to-date (",
  as.character(mortality_df_dates[, 1]),
  "-",
  as.character(mortality_df_dates[, 2]),
  ")"
)

# save to pin board
mortality_folder %>%
  pin_write(
    x = mortality_data,
    name = mortality_df_name,
    title = mortality_df_title,
    type = "rds",
    description = str_c(
      "Finalized and year-to-date all cause mortality data extract provided by AZDHS. Includes data for Coconino County residents or deaths occurring in Coconino County. (",
      as.character(mortality_df_dates[, 1]),
      "-",
      as.character(mortality_df_dates[, 2]),
      ")"
    ),
    metadata = list(
      owner = "Coconino HHS",
      department = "Epidemiology",
      user = "rherrera"
    )
  )

